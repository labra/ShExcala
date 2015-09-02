package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shacl.Shacl._
import es.weso.shacl._
import es.weso.shacl.Typing._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.rdf.PrefixMap
import scala.util.parsing.input.Positional
import es.weso.rdf._
import es.weso.shex.Context._
import scala.util.matching.Regex
import es.weso.utils.Logging
import es.weso.utils.Boolean
import scala.util._
import es.weso.shacl.ShaclDoc._
import es.weso.utils.PrefixMapUtils._
// import treelog.LogTreeSyntaxWithoutAnnotations._
import es.weso.rdf.validator.RDFValidator

case class ValidationException(msg:String) extends Exception {
  override def toString = "ValidationException: " + msg 
}

trait ShaclValidator   
 extends Logging {


  
}

