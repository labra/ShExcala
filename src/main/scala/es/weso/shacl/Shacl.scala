package es.weso.shacl

import scala.util.Try

import org.slf4j.LoggerFactory

import converter.RDF2Schema
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.IRI
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.parser.RDFParser
import es.weso.rdf.validator.ValidationAttempt

object Shacl extends RDFParser {

  val log = LoggerFactory.getLogger("Shacl")

  // TODO: move elsewhere...
  lazy val emptyInclPropSet: List[IRI] = List()
  
  def validateRDF(rdf: RDFReader): Try[Seq[ValidationAttempt[RDFNode,Label]]] = {
    for {
      (schema,pm) <- RDF2Schema.rdf2Schema(rdf)
    } yield {
      val matcher = ShaclMatcher(schema,rdf)
      matcher.validateAttempts
    }
  }
}
