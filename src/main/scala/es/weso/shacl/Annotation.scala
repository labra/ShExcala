package es.weso.shacl
import scala.util.parsing.input.Positional
import es.weso.rdfgraph.nodes._

case class Annotation(
    iri: IRI,
    value: Either[IRI, Literal])
