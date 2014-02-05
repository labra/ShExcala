package es.weso.shex

import es.weso.rdfNode.IRI
import es.weso.rdfNode.RDFNode

trait ShExResult

case class Pass(assignment: Map[RDFNode,IRI]) extends ShExResult {

  def assign(node: RDFNode, iri: IRI): ShExResult = {
       Pass(assignment = assignment + (node -> iri))
  }

}

case class NoPass() extends ShExResult

