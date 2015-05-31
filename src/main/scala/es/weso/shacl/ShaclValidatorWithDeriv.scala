package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shacl.Shacl._
import es.weso.shacl.Typing._
import es.weso.monads.Result._
import es.weso.rdf._
import es.weso.rdf._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils.Logging
import es.weso.monads._

/**
 * Shape validator using Regular Expression Derivatives
 * Some parts of this code have been inspired by:
 * https://hackage.haskell.org/package/hxt-regex-xmlschema-9.1.0/docs/src/Text-Regex-XMLSchema-String-Regex.html
 *
 */

