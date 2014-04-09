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
import es.weso.rdfNode._
import scala.util.Try

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

  /**
   * Main entry point for parser
   *
   */
  def schemaParser(s: ShapeParserState): Parser[(Schema, ShapeParserState)] =
    shExParser(s) ^^
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

  // TODO: Add start
  def statement(s: ShapeParserState): Parser[(Option[Shape], ShapeParserState)] =
    (directive(s) <~ opt(WS) ^^ { case s1 => (None, s1) }
      | shape(s) ^^ { case (sh, s1) => (Some(sh), s1) }
      | start(s) ^^ { case s1 => (None, s1) })

 
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
    opt(WS) ~> label(s) ~ typeSpec(s) ^^ { case (l ~ r) => (Shape(l, r._1), r._2) }

  def typeSpec(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    opt(WS) ~> "{" ~> opt(WS) ~> opt(orExpression(s)) <~ opt(WS) <~ "}" ^^ 
      { case None           => (NoRule,s) 
        case Some((ors,s1)) => (ors,s1) 
      }
  }

  // Curried repState --- TODO: move to library
  def repS[T,S](p: S => Parser[(T,S)])
             (s:S): Parser[(List[T],S)] = 
	  repState(s,p)

  def orExpression(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    seqState(andExpression,repS(arrowState(orExpression,symbol("|"))))(s) ^^ 
      { case (p,s1) => (p._2.foldLeft(p._1){case (x, r) => OrRule(x,r)},s1) }
  }
  
 
  def andExpression(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
	seqState(unaryExpression,repS(arrowState(andExpression,symbol(","))))(s) ^^ 
      { case (p,s1) => (p._2.foldLeft(p._1){case (x, r) => AndRule(x,r)}, s1) }
  }
  
  def unaryExpression(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    ( arc(s) 
    | symbol("(") ~> orExpression(s) <~ symbol(")") 
    )
  }

  def label(s: ShapeParserState): Parser[Label] = {
    iri(s.namespaces) ^^ { case iri => IRILabel(iri) }
    // TODO: Add possibility of BNode
  }

  // TODO: Add not and reverse
  // TODO: add repeatCount and CODE
  def arc(s: ShapeParserState): Parser[(Rule, ShapeParserState)] = {
    nameClassAndValue(s) ~ opt(repeatCount) ^^
      {
        case ((n, v), s1) ~ Some(c) => (ArcRule(None, n, v, c, NoActions), s1)
        case ((n, v), s1) ~ None    => (ArcRule(None, n, v, Default, NoActions), s1)
      }
  }
  
  def repeatCount: Parser[Cardinality] = {
   ( symbol("*") ^^^ Star
   | symbol("+") ^^^ Plus
   | symbol("?") ^^^ Opt
   )
    // TODO: integer ranges 
  }
  // TODO: add Any, Stem
  def nameClassAndValue(s: ShapeParserState): Parser[((NameClass, ValueClass), ShapeParserState)] = {
    iri(s.namespaces) ~ fixedValues(s) ^^ { case (i ~ v) => ((NameTerm(i), v._1), v._2) }
  }

  // TODO: add typeSpec ?
  def fixedValues(s: ShapeParserState): Parser[(ValueClass, ShapeParserState)] = {
    opt(WS) ~>
      (token("@") ~> label(s) ^^ { case l => (ValueReference(l), s) }
        | valueSet(s)
        | valueObject(s) ^^ { case (o, s) => (ValueType(o), s) }) <~ opt(WS)
  }

  def valueSet(s: ShapeParserState): Parser[(ValueSet, ShapeParserState)] =
    (openParen ~>
      rep1sepState(s, valueObject, WS)
      <~ closeParen) ^^ { case (ls, s) => (ValueSet(ls), s) }

  def openParen: Parser[String] = symbol("(") // opt(WS) ~> "(" <~ opt(WS)
  def closeParen: Parser[String] = symbol(")")

  /**
   * It corresponds to object rule in
   *  [[http://www.w3.org/2013/ShEx/ShEx.bnf grammar]]
   */
  def valueObject(s: ShapeParserState): Parser[(RDFNode, ShapeParserState)] =
    opt(WS) ~>
      (iri(s.namespaces) ^^ { case iri => (iri, s) }
        | BlankNode(s.bNodeLabels) ^^ { case (id, table) => (id, s.newTable(table)) }
        | literal(s.namespaces) ^^ { case l => (l, s) }) <~ opt(WS)

  // Parsing symbols skipping spaces...
  // TODO: should refactor to other file 
  def symbol(str: Parser[String]): Parser[String] = {
    opt(WS) ~> str <~ opt(WS)
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
        case Success(x, _) => scala.util.Success((x._1,x._2.namespaces))
        case NoSuccess(msg, _) => scala.util.Failure(new Exception(msg))
      }
    } catch {
      case e: Exception => scala.util.Failure(e)
    }
  }


}