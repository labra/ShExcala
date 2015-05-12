package es.weso.shacl

import es.weso.parser._
import es.weso.shacl.Shacl._
import es.weso.rdf._
import es.weso.shex.PREFIXES._
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
    shaclSchemaParser(s) <~ opt(WS) ^^
      { case (shaclSchema, s) => (Schema(s.namespaces, shaclSchema), s) }
    
  def shaclSchemaParser(s: ShapeParserState): Parser[(SHACLSchema, ShapeParserState)] =
    opt(WS) ~> repState(s, statement) ^^ {
      case (lsOpt, s1) => {
        val startLabel =
          if (s1.starts.isEmpty) None
          else Some(IRILabel(s1.starts.last))
        (SHACLSchema(
            id = None,
            rules = lsOpt.flatten,
            start = startLabel), s1)
      }
    } 
    
    def statement(s: ShapeParserState): Parser[(Option[Rule], ShapeParserState)] =
    (directive(s) <~ opt(WS) ^^ { case s1 => (None, s1) }
      | rule(s) ^^ { case (rule, s1) => (Some(rule), s1) }
      | start(s) ^^ { case s1 => (None, s1) }
    )

  def directive(s: ShapeParserState): Parser[ShapeParserState] =
    ( prefixDirective(s)
    | baseDirective(s)
    )

  def start(s: ShapeParserState): Parser[ShapeParserState] = {
    token("start") ~> opt(WS) ~> "=" ~> opt(WS) ~>
      (label(s) ^^ {
        case IRILabel(iri) => s.addStart(iri)
        case _ => ???
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
    opt(WS) ~> label(s) ~ (opt(WS) ~> shapeDefinition(s)) ^^
      { case (l ~ next) => (Rule(l, next._1,noExtension), next._2) }
  
  def label(s: ShapeParserState): Parser[Label] = {
     iri(s.namespaces) ^^ { case iri => IRILabel(iri) }
    // TODO: Add possibility of BNode
  }

  def shapeDefinition(s: ShapeParserState): Parser[(ShapeDefinition, ShapeParserState)] =
     ???
     
  def typeSpec(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    "{" ~> opt(WS) ~> opt(choiceExpr(s)) <~ opt(WS) <~ "}" ^^
      {
        case None => (EmptyShape, s)
        case Some((ors, s1)) => (ors, s1)
      }
  }

  def choiceExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    seqState(sequenceExpr,
             repS(arrowState(choiceExpr,symbol("|")))
    )(s) ^^
      {
        case (shape ~ shapes, s1) => (
          OneOfShape(shape :: shapes),
          s1
        )
      }
    
  }
     
  def sequenceExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    seqState(unaryExpr,
             repS(arrowState(sequenceExpr,symbol(",")))
    )(s) ^^
      {
        case (shape ~ shapes, s1) => (
          GroupShape(shape :: shapes),
          s1
        )
      }
  }
  
def unaryExpr(s: ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
    ( arc(s)
    | symbol("(") ~> choiceExpr(s) <~ symbol(")")  // TODO: add cardinalities to groups?
    ) 
}
    
def arc(s:ShapeParserState): Parser[(ShapeExpr, ShapeParserState)] = {
  opt(symbol("^")) ~ predicate ~ opt(repeatCount) ^^    
}

def predicate(s: ShapeParserState): Parser[((IRI, ValueClass), ShapeParserState)] =
  (iri(s.namespaces) ~ fixedValues(s) ^^ {
      case (is ~ ((v, s))) => {
        if (is.isStem) ((NameStem(is), v), s)
        else ((NameTerm(is.iri), v), s)
      }
    }
| symbol("a") ~> fixedValues(s) ^^ {
        case (v, s1) => ((NameTerm(rdf_type), v), s1)
}
)



def repeatCount: Parser[Cardinality] = {
    ( symbol("*") ^^^ star 
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

def dot = symbol(".")

def integer: Parser[Int] = {
    """\d\d*""".r ^^ (s => s.toInt)
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