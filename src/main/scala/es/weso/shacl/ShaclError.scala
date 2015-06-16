package es.weso.shacl

import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Shacl._


sealed abstract class ShaclError

case class FailedTripleConstraint(
    root: RDFNode, 
    predicate: IRI, 
    obj: RDFNode, 
    tripleConstraint: TripleConstraint) extends ShaclError
    
case class FailedEmptyCondition(
    root:RDFNode) extends ShaclError
    
case class NotFoundShape(label: Label) extends ShaclError