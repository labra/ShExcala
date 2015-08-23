package es.weso.rdf.validator

import es.weso.rdf._;
import es.weso.rdfgraph.nodes._
import es.weso.monads.Result
import es.weso.shex.Typing

trait RDFValidator {
  type Label
  type ValidationResult 
  type ValidationSchema 
  type Node = RDFNode

  def id: String
  
  val rdf: RDFReader
  val schema: ValidationSchema
  
  def subjects: List[RDFNode] = rdf.subjects.toList
  def labels: List[Label]
  def resultLabels(node: Node, result: ValidationResult): Set[Label] 

  def match_node_label(node: Node)(label: Label): Result[ValidationResult]
  
  def match_label_node(label: Label)(node: Node): Result[ValidationResult] = {
    match_node_label(node)(label)
  }
  
  def match_node_AllLabels(node:Node): Result[ValidationResult] = {
    Result.passSome(labels.toList, match_node_label(node))
  }
  
  def combine(t1: ValidationResult, t2: ValidationResult): ValidationResult 

  def matchAllNodes_Label(lbl: Label): Result[ValidationResult] = {
    Result.combineAll(subjects, match_label_node(lbl), combine)
  }

  def matchAllNodes_AllLabels: Result[ValidationResult] = {
    Result.combineAll(subjects, match_node_AllLabels, combine)
  }
 
}

