package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.rdfNode.IRI
import es.weso.parser.PrefixMap
import es.weso.parser.TurtleParser
import scala.util.parsing.combinator.RegexParsers
import es.weso.parser.StateParser
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import es.weso.parser.W3cTokens
import es.weso.parser.TurtleParserState
import es.weso.rdfNode.RDFNode


trait ShapeParser 
	extends Positional 
	with RegexParsers 
	with StateParser
	with W3cTokens 
	with TurtleParser {

	def shExDoc(implicit s: ShapeParserState) : 
	  		Parser[ResultParser[ShEx,ShapeParserState]] = 
     positioned ( opt(WS) ~> repState(s,statement) ^^ 
     	{ case (lss,s) => ResultParser(shEx(lss.flatten),s) }
     )

    def shEx(ls: List[Shape]): ShEx = {
	  ShEx(ls,None)
	}
	
  def statement(s:ShapeParserState): 
	  		Parser[(List[Shape],ShapeParserState)] = 
     ( directive(s) <~ opt(WS) ^^ { case s1 => (List(),s1) }
     | shapes(s) 
     )

  def directive (s:ShapeParserState) : Parser[ShapeParserState] = 
    ( prefixDirective(s) 
    | baseDirective(s)
    )
  
  def baseDirective (s: ShapeParserState) : Parser[ShapeParserState] = {
    (SPARQLBase | baseId ) ^^ {
      case (iri) => s.newBase(s.baseIRI.resolve(iri))
    }
  }

  def prefixDirective (s: ShapeParserState) : Parser[ShapeParserState] = {
    (SPARQLPrefix | prefixId ) ^^ {
      case (prefix,iri) => s.addPrefix(prefix, iri)
    }
  }

  def shapes(s:ShapeParserState) : 
	  Parser[(List[Shape],ShapeParserState)] = 
    repState(s,shape) ^^ { case (ss,s1) => (ss,s1)}
    
  def shape(s:ShapeParserState): Parser[(Shape,ShapeParserState)] = 
    label(s) ~ ruleSpec(s) ^^ { case (l ~ r) => (Shape(l,r._1),r._2) }
    
  def label(s:ShapeParserState): Parser[Label] = {
    iri(s.namespaces) ^^ { case iri => IRILabel(iri)}
    // TODO: Add possibility of BNode
  }

  def ruleSpec(s:ShapeParserState): Parser[(Rule,ShapeParserState)] = 
    opt(WS) ~> token("{") ~> opt(WS) ~> typeSpec(s) <~ opt(WS) <~ token("}") 
    
  def typeSpec(s:ShapeParserState): Parser[(Rule,ShapeParserState)] = {
    arc(s)
    // TODO add: include and Group with OrExpression
  }
   
 // TODO: Add not and reverse
 // TODO: add repeatCount and CODE
 def arc(s:ShapeParserState): Parser[(Rule,ShapeParserState)] = {
   nameClassAndValue(s) ^^ 
      { 
        case ((n,v),s1) => (ArcRule(None, n, v, Default, NoActions),s1)
      } 
 }
 
 // TODO: add Any, Stem
 def nameClassAndValue(s: ShapeParserState): Parser[((NameClass,ValueClass), ShapeParserState)] = {
   iri(s.namespaces) ~ fixedValues(s) ^^ { case (i ~ v) => ((NameTerm(i),v._1),v._2) }  
 } 

 // TODO: add typeSpec ?
 def fixedValues(s: ShapeParserState): Parser[(ValueClass,ShapeParserState)] = {
  ( token("@") ~> label(s) ^^ { case l => (ValueReference(l),s)}
  | valueSet(s) 
  | valueObject(s) ^^ { case (o,s) => (ValueType(o),s)}
  )
 }

 def valueSet(s: ShapeParserState): Parser[(ValueSet,ShapeParserState)] = 
   rep1sepState(s, valueObject,token(",")) ^^ { case (ls,s) => (ValueSet(ls),s)}
 
 
 // It corresponds to object rule in grammar
 def valueObject(s: ShapeParserState): Parser[(RDFNode, ShapeParserState)] = 
  opt(WS) ~>
  ( iri(s.namespaces) ^^ { case iri => (iri,s)}
  | BlankNode(s.bNodeLabels) ^^ { case (id,table) => (id,s.newTable(table)) }
  | literal(s.namespaces) ^^ { case l => (l,s) }
  ) <~ opt(WS)
 
 
}

object ShapeParser extends ShapeParser {
  
   /**
   * Parse a string with a base IRI
   * @param s: input string
   * @param baseIRI: Initial Base IRI
   * @return Left(rs) = list of shapes successfully parsed
   *         Right(msg) = Error msg
   */
  def parse(s:String, baseIRI: IRI = IRI("")) : Either[ShEx,String] = {
    try {
     parseAll(shExDoc(ShapeParserState.initial.newBase(baseIRI)),s) match {
      case Success(ResultParser(x,_),_) => Left(x)
      case NoSuccess(msg,_) 		    => Right(msg)
     }
    } catch {
      case e: Exception => Right("Exception during parsing: " + e)
    }
  }

}