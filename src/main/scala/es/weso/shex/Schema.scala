package es.weso.shex

import es.weso.rdfNode.IRI
import es.weso.parser.PrefixMap
import es.weso.rdfGraph.RDFGraph
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeParser._
import scala.util.parsing.input.Positional
import scala.util.{Try, Success, Failure}

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 * 
 */

case class Schema(pm: PrefixMap, shEx: ShEx) extends Positional {

  override def toString(): String = {
    val sd = ShapeDoc(pm)
    sd.schema2String(this)
  }
  

}

object Schema {

  def fromString(cs: CharSequence): Try[(Schema,PrefixMap)] = {
    ShapeParser.parse(cs) 
  }  

  def matchShape(schema: Schema, graph: RDFGraph) : ShExResult = ???
}
