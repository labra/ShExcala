package es.weso.shacl

import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shacl.PREFIXES._
import util._
import es.weso.utils.PrefixMapUtils._
import org.slf4j._
import es.weso.utils.{
  Success => UtilsSuccess, //TODO: Check why utils exports Success and remove it 
  _
}
import Checker._
import NumericFacetTypeClass._
import es.weso.rdf.validator._
import converter.RDF2Schema
import es.weso.rdf.parser.RDFParser
import es.weso.shacl.Label._

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
