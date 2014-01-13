package es.weso.shex

import es.weso.rdfNode.IRI
import es.weso.parser.PrefixMap
import es.weso.shex.ShapeSyntax._

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 * 
 */

case class Schema(pm: PrefixMap, rules: Seq[Shape]) {

  def toString(s:Schema): String = {
    val sd = ShapeDoc(pm)
    sd.rules2String(rules)
  }

}

object Schema {
  
}
