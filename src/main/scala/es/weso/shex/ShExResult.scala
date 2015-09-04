package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.monads._
import es.weso.monads.Result._
import es.weso.rdf.validator.ValidationResult
import es.weso.shex.ShapeSyntax.Label

case class ShExResult(value: Result[Typing]) extends ValidationResult[RDFNode,Label] {
  override def emptyResult = Passed(Stream())
  
  override def combine(other: ValidationResult[RDFNode,Label]): ValidationResult[RDFNode,Label] = {
    other match {
      case ShExResult(r) => ??? // ShExResult(value.combine(r))
      case _ => throw new Exception("combine: ShExResult...unexpected: " + other)
    }
  }
  
  override def getNegativeLabels(node: RDFNode): Seq[Label] = Seq()
  
  override def getPositiveLabels(node: RDFNode): Seq[Label] = {
    if (append.isValid).... 
  }
}

/*case class Pass(assignment: Map[RDFNode, IRI]) extends ShExResult {

  def assign(node: RDFNode, iri: IRI): ShExResult = {
    Pass(assignment = assignment + (node -> iri))
  }

}

case class NoPass() extends ShExResult

*/