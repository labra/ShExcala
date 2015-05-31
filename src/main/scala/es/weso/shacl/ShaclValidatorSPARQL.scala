package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.rdf.PrefixMap
import es.weso.rdf._
import es.weso.shex.Context._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils.Logging
import es.weso.monads.Passed
import es.weso.monads.Failure

/**
 * ShACL validator converting to SPARQL
 */
trait ShaclValidatorSPARQL extends ShaclValidator with Logging {

  override def id = "Validator based on SPARQL"

}

/*object ShaclValidatorSPARQL extends ShaclValidator {

}*/