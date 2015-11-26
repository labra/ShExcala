package es.weso.rdf.validator
import util._
import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Label

trait ValidationAttempt[Node,Label] {
  def isValid : Boolean
  
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
  
  override def isValid: Boolean = {
    result.isValid
  }
  
  override def show(verbose: Boolean, cut: Int = 1, prefixMap: PrefixMap = PrefixMap.empty): String = {
    val sb: StringBuilder = new StringBuilder
    if (result.isValid) {
       sb ++= s"$node has shape $shape"
      if (verbose) {
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
  
  override def isValid = false 
  
  override def show(verbose: Boolean, cut: Int = 1, prefixMap: PrefixMap = PrefixMap.empty): String = {
    s"ScopeClassAttempt: $node with $cls "
  }
}

object ValidationAttempt {
  
  def showAttempts[Node,Label](
      attempts: Seq[ValidationAttempt[Node,Label]],
      verbose: Boolean,
      cut: Int,
      pm: PrefixMap): String = {
    if (attempts.isEmpty) {
      "No declared validation detected (sh:scopeNode/sh:scopeClass)"
    } else {
      val sb = new StringBuilder
      for (attempt <- attempts) {
        sb ++= attempt.show(verbose, cut, pm) + "\n"
      }
      sb.toString
    }
  }
}

