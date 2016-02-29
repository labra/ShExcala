package es.weso.shacl
import scala.util.parsing.input.Positional
import es.weso.rdf.nodes._

case class Annotation(
    iri: IRI,
    value: Either[IRI, Literal])
