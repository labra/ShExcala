package es.weso.shex

import scala.util.Try

import org.slf4j.LoggerFactory

import converter.RDF2Schema
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.IRI
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.parser.RDFParser
import es.weso.rdf.validator.ValidationAttempt

/**
 *  
 */
object ShEx {

  // TODO: move elsewhere...
  // lazy val emptyInclPropSet: List[IRI] = List()
  
  /**
   * validates a single RDF without a Schema
   * 
   * It retrieves the Schema directly from the RDF 
   */
  def validateRDF(rdf: RDFReader): Try[Seq[ValidationAttempt[RDFNode,Label]]] = {
    for {
      (schema,pm) <- RDF2Schema.rdf2Schema(rdf)
    } yield {
      val matcher = ShExMatcher(schema,rdf)
      matcher.validateAttempts
    }
  }
}
