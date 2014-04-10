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
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.shex.ShapeSyntax.Label


case class ShapeParserState (
  val namespaces : PrefixMap ,
  val bNodeLabels : BNodeTable,
  val starts: List[Label],
  val baseIRI: IRI
  ) {
 
 def newTable(table: BNodeTable) : ShapeParserState = 
   copy(bNodeLabels=table)

 def addPrefix(prefix: String, iri: IRI) : ShapeParserState = 
   copy(namespaces = namespaces.addPrefix(prefix, iri))

 def newBNode : (BNodeId,ShapeParserState) = { 
   val (id,t) = bNodeLabels.newBNode 
   (id,copy(bNodeLabels=t))
 }
 
 def newBNode(name: String) : (BNodeId,ShapeParserState) = {
   val (id,t) = bNodeLabels.getOrAddBNode(name)  
   (id,copy(bNodeLabels = t))
 }
 
 def newBase(newIRI:IRI) =
   copy(baseIRI=newIRI)

 def addStart(label: Label) = 
   copy(starts = starts :+ label)

}

object ShapeParserState {
  
  def initial : ShapeParserState = initial(IRI(""))
  def initial(baseIRI : IRI) = 
    ShapeParserState(PrefixMap.empty,BNodeTable.empty,List(),baseIRI)
  
}