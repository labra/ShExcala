package es.weso.shex

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.Positional
import scala.util.parsing.input._
import util.parsing.input.CharSequenceReader.EofCh
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.immutable.Map
import scala.language.postfixOps
import es.weso.parser._
import es.weso.rdfNode._
import es.weso.shex.ShapeSyntax.Label


case class ShapeParserState (
  val namespaces : PrefixMap ,
  val bNodeLabels : BNodeTable,
  val starts: List[Label],
  val baseIRI: IRI
  ) {
 
 def newTable (table: BNodeTable) : ShapeParserState = 
   ShapeParserState(namespaces,table,starts,baseIRI)

 def addPrefix(prefix: String, iri: IRI) : ShapeParserState = 
   ShapeParserState(namespaces.addPrefix(prefix, iri),bNodeLabels,starts,baseIRI)

 def newBNode : (BNodeId,ShapeParserState) = { 
   val (id,t) = bNodeLabels.newBNode 
   (id,ShapeParserState(namespaces,t,starts,baseIRI))
 }
 
 def newBNode(name: String) : (BNodeId,ShapeParserState) = {
   val (id,t) = bNodeLabels.getOrAddBNode(name)  
   (id,ShapeParserState(namespaces,t,starts,baseIRI))
 }
 
 def newBase(newIRI:IRI) =
   ShapeParserState(namespaces,bNodeLabels,starts,newIRI)

 def addStart(label: Label) = 
   ShapeParserState(namespaces,bNodeLabels,starts :+ label,baseIRI)

}

object ShapeParserState {
  
  def initial : ShapeParserState = initial(IRI(""))
  def initial(baseIRI : IRI) = 
    ShapeParserState(PrefixMap.empty,BNodeTable.empty,List(),baseIRI)
  
}