package es.weso.shacl

import es.weso.rdfgraph.nodes.RDFNode
import es.weso.shacl.Shacl._



trait Tracer {
  def newAttempt(attempt: Attempt): Tracer
  
  def failCurrentAttempt(reason: String): Tracer
}

case class TracerImpl(attempts: Seq[(Attempt, Option[String])]) extends Tracer {
  override def newAttempt(attempt: Attempt) = { 
    if (attempts.isEmpty) {
      TracerImpl(List((attempt,None)))      
    } else {
      TracerImpl(attempts)
    }
  }
  
  override def failCurrentAttempt(reason: String) = {
    ???
  }
}

trait Attempt

case class NodeLabelMatch(node: RDFNode, label: Label)

