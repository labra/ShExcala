package es.weso.shex

import es.weso.rdfNode.IRI
import es.weso.parser.PrefixMap
import es.weso.rdfGraph.RDFGraph
import es.weso.shex.ShapeSyntax._

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 * 
 */

case class Schema(pm: PrefixMap, rules: Seq[Shape]) {

  override def toString(): String = {
    val sd = ShapeDoc(pm)
    sd.schema2String(this)
  }

}

object Schema {
  
  def matchShape(schema: Schema, graph: RDFGraph) : ShExResult = ???
}
