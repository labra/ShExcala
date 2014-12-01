package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.parser._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import org.slf4j._

/**
 * Shape parser. This parser follows
 *  [[http://www.w3.org/2013/ShEx/ShEx.bnf this grammar]]
 *
 *  More info: [[http://www.w3.org/2013/ShEx/Definition.html ShEx Definition]]
 *
 */
trait ShapeParser
  extends Positional
  with RegexParsers
  with StateParser
  with W3cTokens
  with TurtleParser {

  val log = LoggerFactory.getLogger("ShapeParser")
  /**
   * Main entry point for parser
   *
   */
  def schemaParser(s: ShapeParserState): Parser[(Schema, ShapeParserState)] =
    shExParser(s) <~ opt(WS) ^^
      { case (shEx, s) => (Schema(s.namespaces,shEx), s) }


  def shExParser(s: ShapeParserState): Parser[(ShEx, ShapeParserState)] =
    opt(WS) ~> repState(s, statement) ^^ {
      case (lsOpt, s1) => {
        val startLabel = if (s1.starts.isEmpty) None
        			     else Some(s1.starts.last)
        (ShEx(rules= lsOpt.flatten, start=startLabel), s1)
      }
    }

  def shEx(ls: List[Shape]): ShEx = {
    ShEx(ls, None)
  }

  def statement(s: ShapeParserState): Parser[(Option[Shape], ShapeParserState)] =
    ( directive(s) <~ opt(WS) ^^ { case s1 => (None, s1) }
    | shape(s) ^^ { case (sh, s1) => (Some(sh), s1) }
    | start(s) ^^ { case s1 => (None, s1) }
    )

 
  def directive(s: ShapeParserState): Parser[ShapeParserState] =
    (prefixDirective(s)
      | baseDirective(s))

  def start(s: ShapeParserState): Parser[ShapeParserState] = {
    token("start") ~> opt(WS) ~> "=" ~> opt(WS) ~>
      (label(s) ^^ { case lbl => s.addStart(lbl) } // TODO: add typeSpec 
      )
  }

  def baseDirective(s: ShapeParserState): Parser[ShapeParserState] = {
    (SPARQLBase | baseId) ^^ {
      case (iri) => s.newBase(s.baseIRI.resolve(iri))
    }
  }

  def prefixDirective(s: ShapeParserState): Parser[ShapeParserState] = {
    (SPARQLPrefix | prefixId) ^^ {
      case (prefix, iri) => s.addPrefix(prefix, iri)
    }
  }

  def shape(s: ShapeParserState): Parser[(Shape, ShapeParserState)] =
    opt(WS) ~> label(s) ~ ( opt(WS) ~> shapeSpec(s)) ^^ 
        { case (l ~ r) => (Shape(l, r._1), r._2) }

  def shapeSpec(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    openShapeSpec(s) | closedShapeSpec(s)
  }
    
  def openShapeSpec(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    "{" ~> opt(WS) ~> opt(orExpression(s)) <~ opt(WS) <~ "}" ^^ 
      { case None           => (StarRule(AnyRule),s) 
        case Some((ors,s1)) => (OpenRule(ors),s1) 
      }
  }

  def closedShapeSpec(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    "[" ~> opt(WS) ~> opt(orExpression(s)) <~ opt(WS) <~ "]" ^^ 
      { case None           => (EmptyRule,s) 
        case Some((ors,s1)) => (ors,s1) 
      }
  }

  def orExpression(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    seqState(andExpression,
             repS(arrowState(orExpression, 
                             symbol("|")
                            )
                 )
            )(s) ^^ 
      { case (p,s1) => (
           p._2.foldLeft(p._1)
                { case (x, r) => OrRule(x,r) },
           s1
          ) 
      }
  }
  
 
  def andExpression(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
	seqState(unaryExpression,repS(arrowState(andExpression,symbol(","))))(s) ^^ 
      { case (p,s1) => (p._2.foldLeft(p._1){case (x, r) => AndRule(x,r)}, s1) }
  }

  // TODO: Add repeatCount and CODE
  def unaryExpression(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    ( anyRule ^^ { case r => (r,s) }
    | arc(s)
    | symbol("!") ~> unaryExpression(s) ^^ {
      case (r,s1) => (NotRule(r),s1)
      }
    | symbol("(") ~> orExpression(s) <~ symbol(")")
    ) ~ opt(repeatCount) ^^ {
      case (r,s1) ~ Some(fn) => (fn(r),s1)
      case (r,s1) ~ None     => (r,s1)
    }
  }

  def label(s: ShapeParserState): Parser[Label] = {
    iri(s.namespaces) ^^ { case iri => IRILabel(iri) }
    // TODO: Add possibility of BNode
  }

  private def mkArcRule(nvs: ((NameClass, ValueClass), ShapeParserState)): (Rule,ShapeParserState) = {
    val ((n,v),s) = nvs 
    (ArcRule(None, n, v), s)
  }
  
  private def mkRevArcRule(nvs: ((NameClass, ValueClass), ShapeParserState)): (Rule,ShapeParserState) = {
    val ((n,v),s) = nvs 
    (RevArcRule(None, n, v), s)
  }

  private def mkRelationRule(nvs: ((ValueClass, ValueClass), ShapeParserState)): (Rule,ShapeParserState) = {
    val ((v1,v2),s) = nvs 
    (RelationRule(None, v1, v2), s)
  }

  def arc(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    ( symbol("^") ~> nameClassAndValue(s) 
        ^^ { case ((n,v),s1) => (RevArcRule(None,n,v),s1) }
    | symbol("<>") ~> fixedValues(s) ~ fixedValues(s) 
        ^^ { case ((v1,_)) ~ ((v2,_)) => (RelationRule(None,v1,v2),s) }
    | nameClassAndValue(s) 
        ^^ { case ((n,v),s1) => (ArcRule(None,n,v),s1) }
    )
  }

  def anyRule : Parser[Rule] = {
    dot ~ WS ~ dot ~ opt(WS) ~ symbol("*") ^^^ StarRule(AnyRule)
  }
  
  def repeatCount: Parser[Rule => Rule] = {
   ( symbol("*") ^^^ star _
   | symbol("+") ^^^ PlusRule
   | symbol("?") ^^^ option _
   | symbol("{") ~> integer ~ 
       opt(symbol(",") ~> integer) <~ symbol("}") ^^
        { case m ~ maybeN => 
            (m, maybeN) match {
              case (m,None) => rangeMin(m)
              case (m,Some(n)) => {
                  // TODO: Add error checking here? (m > n...)
            	  range(m,n)
              }
            }
        }
   )
  }
  
  def integer: Parser[Int] = {
    """\d\d*""".r ^^ (s => s.toInt)
  }

  def nameClassAndValue(s: ShapeParserState): 
	  Parser[((NameClass, ValueClass), ShapeParserState)] = 
   ( iriStem(s.namespaces) ~ fixedValues(s) ^^ {
        case (is ~ ((v,s))) => {
           if (is.isStem) ((NameStem(is),v),s)
           else ((NameTerm(is.iri), v), s)
        }
     }
   | symbol("a") ~> fixedValues(s) ^^ { 
     	case (v,s1) => ((NameTerm(rdf_type),v),s1)
     }
   | dot ~ fixedValues(s) ^^ { 
     	case (_ ~ ((v,s1))) => ((NameAny(excl=Set()),v), s1) 
     }
   | exclusions(s) ~ fixedValues(s) ^^ {
        case excls ~ ((v,s1)) => {
          ((NameAny(excls),v), s1)
        }
     }
   )

  def exclusions(s:ShapeParserState): 
	  Parser[Set[IRIStem]] = {
    rep1(exclusion(s)) ^^ { 
      case lsIris => {
        (lsIris.toSet)      
      }
    }
  }
  
  def exclusion(s:ShapeParserState) : Parser[IRIStem] = {
    symbol("-") ~> iriStem(s.namespaces)
  }
  
  def iriStem(ns:PrefixMap): Parser[IRIStem] = {
    iri(ns) ~ opt(symbol("~")) ^^ 
       { 
         case iri ~ None => IRIStem(iri,false)
         case iri ~ Some(_) => IRIStem(iri,true)
       }
  }
  
  def dot = symbol(".") 

  def fixedValues(s: ShapeParserState): Parser[(ValueClass, ShapeParserState)] = {
    opt(WS) ~>
      ( token("@") ~> label(s) ^^ { case l => (ValueReference(l), s) }
      | dot ^^^ (ValueAny(excl = Set()),s)
      | valueSet(s)
      | valueType(s) 
      ) <~ opt(WS)
  }

  def valueType(s: ShapeParserState): Parser[(ValueType,ShapeParserState)] = {
    ( opt(WS) ~> iri(s.namespaces) <~opt(WS) ^^ {
      case iri => (ValueType(iri),s)
      }
    | ignorecase("Literal") ^^^ { (typeShexLiteral,s) }
    | ignorecase("IRI") ^^^ { (typeShexIRI,s) }
    | ignorecase("BNode") ^^^ { (typeShexBNode,s) }
    | ignorecase("NonLiteral") ^^^ { (typeShexNonLiteral,s) }
    | ignorecase("NonIRI") ^^^ { (typeShexNonIRI,s) }
    | ignorecase("NonBNode") ^^^ { (typeShexNonLiteral,s) }
    )
  }
  
  def valueSet(s: ShapeParserState): Parser[(ValueSet, ShapeParserState)] =
    (openParen ~>
      rep1sepState(s, valueObject, WS)
      <~ closeParen) ^^ { 
    case (ls, s) => (ValueSet(ls), s) 
  }

  def openParen: Parser[String] = symbol("(") 
  def closeParen: Parser[String] = symbol(")")

  /**
   * It corresponds to object rule in
   *  [[http://www.w3.org/2013/ShEx/ShEx.bnf grammar]]
   */
  def valueObject(s: ShapeParserState): Parser[(ValueObject, ShapeParserState)] = { 
      ( symbol("-") ~> basicValueObject(s) ^^ { case ((vo,s)) => (NoObject(vo),s) } 
      | basicValueObject(s)
      )
  }
  
  
  def basicValueObject(s: ShapeParserState): Parser[(ValueObject, ShapeParserState)] =
    opt(WS) ~>
      ( regexChars ~ opt(LANGTAG) ^^ {
        case r ~ None 		=> (RegexObject(r,None),s)
        case r ~ Some(lang) => (RegexObject(r,Some(lang)),s)
      } 
      | iri(s.namespaces) ^^ { 
         case iri => (RDFNodeObject(iri), s) 
      }
      | BlankNode(s.bNodeLabels) ^^ { 
          case (id, table) => {
             (RDFNodeObject(id), s.newTable(table)) 
          }
        }
      | literal(s.namespaces) ^^ { 
        case l => (RDFNodeObject(l), s) 
        }
      | LANGTAG ^^ { 
        case lang => (LangObject(lang),s) 
      }
      ) <~ opt(WS)

  // Parsing symbols skipping spaces...
  // TODO: should refactor to other file 
  def symbol(str: Parser[String]): Parser[String] = {
    opt(WS) ~> str <~ opt(WS)
  }
      
  // The trick to parse ignoring cases was taken from: 
  // http://stackoverflow.com/questions/6080437/case-insensitive-scala-parser-combinator
  def ignorecase(str:String): Parser[String] = {
    opt(WS) ~> ("""(?i)\Q""" + str + """\E""").r <~ opt(WS)
  }


  def regexChars : Parser[Regex] = {
    "/" ~> acceptRegex("regex","""[a-zA-Z0-9\.\+\*\(\)\[\]\-\{\}]*""".r) <~ "/"^^ {
      case str => {
        str.r
      }
    } 
  }
    
}

object ShapeParser extends ShapeParser {

  /**
   * Parse a string with a base IRI
   * @param s: input string
   * @param baseIRI: Initial Base IRI
   * @return Left(rs) = list of shapes successfully parsed
   *         Right(msg) = Error msg
   */
  def parse(s: CharSequence, baseIRI: IRI = IRI("")): Try[(Schema, PrefixMap)] = {
    try {
      val state = ShapeParserState.initial.newBase(baseIRI)
      parseAll(schemaParser(state), new CharSequenceReader(s)) match {
        case Success((schema,s), in1) => 
          scala.util.Success((schema,s.namespaces))
        case Error(msg,in1) => 
          scala.util.Failure(new Exception("Error at " + in1.pos + ": " + msg))
        case Failure(msg,in1) => {
          scala.util.Failure(new Exception("Failure at " + in1.pos + ": " + msg))
        }
      }
    } catch {
      case e: Exception => scala.util.Failure(e)
    }
  }


}