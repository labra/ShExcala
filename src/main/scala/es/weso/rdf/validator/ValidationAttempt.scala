package es.weso.rdf.validator
import util._
import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Label

case class ValidationAttempt[Node,Label](
    node: Node, 
    label: Label,
    result: ValidationResult[Node,Label,Throwable]
)