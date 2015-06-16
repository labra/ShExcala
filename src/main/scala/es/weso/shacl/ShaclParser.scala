package es.weso.shacl

import es.weso.parser._
import es.weso.shacl.Shacl._
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import es.weso.shex.ShapeParserState
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
import scala.util.{ Try, Success => UtilSuccess, Failure => UtilFailure }
import org.slf4j._

/**
 * Shacl parser. This parser follows
 * http://www.w3.org/2005/01/yacker/uploads/ShEx2/bnf?markup=html
 */
trait ShaclParser
    extends Positional
    with RegexParsers
    with StateParser
    with W3cTokens
    with TurtleParser {

  val log = LoggerFactory.getLogger("ShapeParser")

  /**
   * Main entry point for parser
   */
  def schemaParser(s: ShapeParserState): Parser[(Schema, ShapeParserState)] =
   shaclSchemaParser(s) ^^
      { case (shaclSchema, s) => 
          (Schema(s.namespaces, shaclSchema), s) 
      }
   
  def shaclSchemaParser(s: ShapeParserState): Parser[(SHACLSchema, ShapeParserState)] =
    opt(WS) ~> repState(s, statement) <~ opt(WS) ^^ {
      case (lsOpt, s1) => {
        val startLabel =
          if (s1.starts.isEmpty) None
          else Some(mkLabel(s1.starts.last))
        (SHACLSchema(
          id = None,
          rules = lsOpt.flatten,
          start = startLabel), s1)
      }
    }

  def statement(s: ShapeParserState): Parser[(Option[Rule], ShapeParserState)] =
    ( directive(s) <~ opt(WS) ^^ { case s1 => (None, s1) }
    | begin(s) ^^ { case (rule,s1) => (Some(rule), s1)}
    | rule(s) ^^ { case (rule, s1) => (Some(rule), s1) }
    | start(s) ^^ { case s1 => (None, s1) }
    )

  def directive(s: ShapeParserState): Parser[ShapeParserState] =
    ( prefixDirective(s)
    | baseDirective(s)
    )

  def begin(s: ShapeParserState): Parser[(Rule,ShapeParserState)] = {
    token("begin") ~> opt(WS) ~> "=" ~> opt(WS) ~> {
     shapeDefinition(s) 
    } ^^{
      case (shape,s1) => {
       (Rule(label=IRILabel(IRI("begin")), 
               shapeDefinition = shape, 
               extensionCondition = Seq()),s1) 
      }
    } 
  }

  def start(s: ShapeParserState): Parser[ShapeParserState] = {
    token("start") ~> opt(WS) ~> "=" ~> opt(WS) ~>
      (label(s) ^^ {
        case (IRILabel(iri),s1) => s1.addStart(iri)
        case (BNodeLabel(b),s1) => s1.addStart(b)
        case unknown => throw new Exception("start: unexpected result of label parser " + unknown) 
      }
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

  def rule(s: ShapeParserState): Parser[(Rule, ShapeParserState)] =
    seqState(label, shapeDefinition)(s) ^^
      { case (l ~ shape,s) => (Rule(l, shape, noExtension), s) }

  def label(s: ShapeParserState): Parser[(Label,ShapeParserState)] = {
    opt(WS) ~> 
    ( iri(s.namespaces) ^^ { 
        case iri => (IRILabel(iri),s) 
      }
    | BlankNode(s.bNodeLabels) ^^ { 
        case ((b,t1)) => (BNodeLabel(b),s.newTable(t1)) 
      }
    )
  }

  def shapeDefinition(s: ShapeParserState): Parser[(ShapeDefinition, ShapeParserState)] =
    opt(WS) ~> 
        ( closedShapeDefinition(s) 
        | openShapeDefinition(s)
        )

  def closedShapeDefinition(s: ShapeParserState): Parser[(ShapeDefinition, ShapeParserState)] =
    ignorecase("CLOSED") ~> typeSpec(s) ^^ {
      case (shape, s1) => (ClosedShape(shape), s1)
    }

  def openShapeDefinition(s: ShapeParserState): Parser[(ShapeDefinition, ShapeParserState)] = {
    // inclPropSet ~ 
    typeSpec(s) ^^ {
      case (shape, s1) => (OpenShape(shape, emptyInclPropSet), s1)
    }
  }

  /*  def inclPropSet(s: ShapeParserState): Parser[(Set[IRI], ShapeParserState)] = {
    ???
  } */

  def typeSpec(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    "{" ~> opt(WS) ~> opt(oneOfExpr(s)) <~ opt(WS) <~ "}" ^^
      {
        case None => (EmptyShape, s)
        case Some((ors, s1)) => (ors, s1)
      }
  }

  def oneOfExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    seqState(sequenceExpr,
      repS(arrowState(someOfExpr, symbol("|")))
    )(s) ^^
      {
        case (shape ~ List(), s1) => (shape, s1)
        case (shape ~ shapes, s1) => (OneOfShape(None,shape :: shapes), s1)
      }

  }

  def someOfExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    seqState(sequenceExpr,
      repS(arrowState(sequenceExpr, symbol("||")))
    )(s) ^^
      {
        case (shape ~ List(), s1) => (shape, s1)
        case (shape ~ shapes, s1) => (OneOfShape(None,shape :: shapes), s1)
      }

  }

  def sequenceExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    seqState(unaryExpr,
      repS(arrowState(sequenceExpr, symbol(",")))
    )(s) ^^
      {
        case (shape ~ List(), s1) => (shape, s1)
        case (shape ~ shapes, s1) => (GroupShape(None,shape :: shapes), s1)
      }
  }

  def unaryExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    (arc(s)
    | ( symbol("(") ~> 
        oneOfExpr(s) <~ 
        symbol(")")
      ) ~ cardinality ^^ {
        case (shape,s1) ~ c => 
          (RepetitionShape(None,shape,c),s1)
      }
    )
  }

  def arc(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    opt(symbol("^")) ~ predicate(s) ~ valueClass(s) ~ cardinality ^^ {
      case (Some(_) ~ p ~ ((s: ShapeConstr, s1)) ~ c) => (InverseTripleConstraint(None, p, s, c), s1)
      case (None ~ p ~ ((v: ValueClass, s1)) ~ c) => (TripleConstraint(None, p, v, c), s1)
    }
  }

  def cardinality: Parser[Cardinality] =
    opt(repeatCount) ^^ {
      case Some(c) => c
      case None => defaultCardinality
    }

  def valueClass(s: ShapeParserState): Parser[(ValueClass, ShapeParserState)] = {
    (ignorecase("LITERAL") ^^^ (LiteralKind, s)
      | ignorecase("IRI") ^^^ (IRIKind, s)
      | ignorecase("BNODE") ^^^ (BNodeKind, s)
      | ignorecase("NONLITERAL") ^^^ (NonLiteralKind, s)
      | ignorecase("ANY") ^^^ (AnyKind, s)
      | dot ^^^ (AnyKind, s)
      | opt(WS) ~> iri(s.namespaces) ~ xsfacets(s) <~ opt(WS) ^^ {
        case iri ~ facets => (LiteralDatatype(iri, facets), s)
      }
      | valueSet(s)
    )
  }

  def valueSet(s:ShapeParserState): Parser[(ValueSet,ShapeParserState)] = 
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
    ( // symbol("-") ~> basicValueObject(s) ^^ { case ((vo, s)) => (NoObject(vo), s) } | 
    basicValueObject(s)
    )
  }

  def basicValueObject(s: ShapeParserState): Parser[(ValueObject, ShapeParserState)] =
    opt(WS) ~> (
/*      (regexChars ~ opt(LANGTAG) ^^ {
        case r ~ None => (RegexObject(r, None), s)
        case r ~ Some(lang) => (RegexObject(r, Some(lang)), s)
      } | */
         iri(s.namespaces) ^^ {
          case iri => (ValueIRI(iri), s)
        }
/*        | BlankNode(s.bNodeLabels) ^^ {
          case (id, table) => {
            (RDFNodeObject(id), s.newTable(table))
          }
        } */
        | literal(s.namespaces) ^^ {
          case l => (ValueLiteral(l), s)
        }
/*        | LANGTAG ^^ {
          case lang => (LangObject(lang), s)
        } */
      ) <~ opt(WS)

  
  def xsfacets(s: ShapeParserState): Parser[Seq[XSFacet]] = {
    repsep(xsfacet(s), WS)
  }

  def xsfacet(s: ShapeParserState): Parser[XSFacet] = {
    ((ignorecase("PATTERN") ~> regexChars) ^^ {
      case regex => Pattern(regex)
    }
      | ignorecase("MININCLUSIVE") ~> integer ^^ { case n => MinInclusive(n) }
      | ignorecase("MINEXCLUSIVE") ~> integer ^^ { case n => MinExclusive(n) }
      | ignorecase("MAXINCLUSIVE") ~> integer ^^ { case n => MaxInclusive(n) }
      | ignorecase("MAXEXCLUSIVE") ~> integer ^^ { case n => MaxExclusive(n) }
      | ignorecase("LENGTH") ~> integer ^^ { case n => Length(n) }
      | ignorecase("MINLENGTH") ~> integer ^^ { case n => MinLength(n) }
      | ignorecase("MAXLENGTH") ~> integer ^^ { case n => MaxLength(n) }
      | ignorecase("TOTALDIGITS") ~> integer ^^ { case n => TotalDigits(n) }
      | ignorecase("FRACTIONDIGITS") ~> integer ^^ { case n => FractionDigits(n) }
    )
  }

  def predicate(s: ShapeParserState): Parser[IRI] =
    (iri(s.namespaces)
      | symbol("a") ^^^ rdf_type
    )

  def repeatCount: Parser[Cardinality] = {
    (symbol("*") ^^^ star
      | symbol("+") ^^^ plus
      | symbol("?") ^^^ optional
      | symbol("{") ~> integer ~
      opt(symbol(",") ~> integer) <~ symbol("}") ^^
      {
        case m ~ maybeN =>
          (m, maybeN) match {
            case (m, None) => UnboundedCardinalityFrom(m)
            case (m, Some(n)) => {
              // TODO: Add error checking here? (m > n...)
              RangeCardinality(m, n)
            }
          }
      }
    )
  }

  def dot = opt(WS) ~> symbol(".") <~ opt(WS)

  def integer: Parser[Int] = {
    """\d\d*""".r ^^ (s => s.toInt)
  }

  def regexChars: Parser[Regex] = {
    "/" ~> acceptRegex("regex", """[a-zA-Z0-9\.\+\*\(\)\[\]\-\{\}]*""".r) <~ "/" ^^ {
      case str => {
        str.r
      }
    }
  }

  // Parsing symbols skipping spaces...
  // TODO: should refactor to other file 
  def symbol(str: Parser[String]): Parser[String] = {
    opt(WS) ~> str <~ opt(WS)
  }

  // The trick to parse ignoring cases was taken from: 
  // http://stackoverflow.com/questions/6080437/case-insensitive-scala-parser-combinator
  def ignorecase(str: String): Parser[String] = {
    opt(WS) ~> ("""(?i)\Q""" + str + """\E""").r <~ opt(WS)
  }

}

object ShaclParser extends ShaclParser {

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
        case Success((schema, s), in1) =>
          UtilSuccess((schema, s.namespaces))
        case Error(msg, in1) =>
          UtilFailure(new Exception("Error at " + in1.pos + ": " + msg))
        case Failure(msg, in1) => {
          UtilFailure(new Exception("Failure at " + in1.pos + ": " + msg))
        }
      }
    } catch {
      case e: Exception => scala.util.Failure(e)
    }
  }

}