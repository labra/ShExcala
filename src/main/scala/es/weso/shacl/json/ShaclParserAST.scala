package es.weso.shacl.json

import es.weso.parser._
import AST._
import es.weso.shacl._
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
 * http://www.w3.org/2005/01/yacker/uploads/ShEx3/bnf?lang=perl
 */
trait ShaclParser
    extends Positional
    with RegexParsers
    with StateParser
    with W3cTokens
    with TurtleParser {

  val log = LoggerFactory.getLogger("ShapeParser")


}