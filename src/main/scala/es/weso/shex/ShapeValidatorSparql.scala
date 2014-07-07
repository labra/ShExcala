package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.parser.PrefixMap
import es.weso.rdf._
import es.weso.shex.Context._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils.Logging
import es.weso.monads.Passed
import es.weso.monads.Failure

/**
 * Shape validator converting to SPARQL queries
 * 
 */
trait ShapeValidatorSparql extends ShapeValidator with Logging {
  
  override def matchRule(ctx: Context, g: Set[RDFTriple], rule: Rule): Result[Typing] = 
    ???
}