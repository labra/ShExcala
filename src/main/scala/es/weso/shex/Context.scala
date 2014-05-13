package es.weso.shex

import es.weso.rdf._
import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.monads.Result
import es.weso.monads.Result._
import es.weso.rdfgraph.nodes.IRI

case class Context(
    rdf: RDF,
    shEx: ShEx) {
  
  def triplesWithSubject(iri:IRI) : Set[RDFTriple] = { 
    rdf.triplesWithSubject(iri)
  }
  
  def getIRIs(): List[IRI] = {
    rdf.iris().toList
  }
  
  def getShapes(): List[Shape] = {
    shEx.rules.toList
  }

  def getShape(label: Label): Result[Shape] = {
    shEx.findShape(label) match {
      case None => failure("Not found shape with label " + label)
      case Some(sh) => unit(sh)
    }
  }
}

object Context {
  def emptyContext : Context = Context(RDFTriples.noTriples,
      ShEx(rules = Seq(),start = None)
      )
      
}
