package es.weso.shacl

import es.weso.monads._
import es.weso.rdf._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import java.lang._
import es.weso.utils.Logging
import es.weso.rdf.validator.RDFValidator
import es.weso.monads.Result._

case class ShaclMatcher(
    schema: Schema, 
    rdf: RDFReader
   ) extends RDFValidator 
        with ShaclValidator {
  
  override def id() = "SHACL validator" 
  
  type Label = Shacl.Label
  type ValidationResult = ValidationState
  type ValidationSchema = Schema
  
  def resultLabels(iri: RDFNode, result: ValidationState): Set[Label] = {
    result.resultLabels(iri)
  }
  
def combine(
    t1: ValidationState,
    t2: ValidationState): ValidationState = {
  t1.combine(t2)
} 
      
def labels = schema.labels

def match_node_label
      (node: RDFNode)
      (label: Label): Result[ValidationState] = {
  val ctx = Context(
    rdf = rdf, 
    schema = schema.shaclSchema, 
    typing = Typing.emptyTyping, 
    pm = schema.pm, 
    pending = List(),
    validateIncoming = true
  )
  for {
//   _ <- setTrace(true)
   vs <- matchNodeLabel(node, label, ctx) 
  } yield vs
} 
 
} 