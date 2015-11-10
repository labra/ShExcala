package es.weso.rdf.validator
import util._
import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Label

class ValidationAttempt[Node,Label] {
  def show(
      verbose: Boolean = false, 
      cut: Int = 1, 
      prefixMap: PrefixMap = PrefixMap.empty): String
}

case class ScopeNodeAttempt[Node,Label] (
    node: Node, 
    shape: Node,
    result: ValidationResult[Node,Label,Throwable]
) extends ValidationAttempt[Node,Label] {
  override def show(verbose: Boolean, cut: Int = 1, prefixMap: PrefixMap = PrefixMap.empty): String = {
    val sb: StringBuilder = new StringBuilder
    if (result.isValid) {
      if (verbose) {
       sb ++= s"$node has shape $shape"
       sb ++= result.show(cut)(prefixMap) 
      }
    } else {
      sb ++= s"Failure: Node $node doesn't have shape $shape"
      if (verbose) {
        sb ++= s"Result: ${result.show(cut)(prefixMap)}"
      } 
    }
    sb.toString
  }
}

case class ScopeClassAttempt[Node,Label] (
    node: Node,
    cls: Node,
    result: ValidationResult[Node,Label,Throwable]
) extends ValidationAttempt[Node,Label] {
  override def show(verbose: Boolean, cut: Int = 1, prefixMap: PrefixMap = PrefixMap.empty): String = {
    s"ScopeClassAttempt: $node with $cls "
  }
}

