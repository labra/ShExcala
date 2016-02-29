package es.weso.rdf.validator

import es.weso.rdf._;
import es.weso.rdf.nodes._
import es.weso.monads.Result
import util._
import es.weso.rdf.PrefixMap
import es.weso.shacl.PREFIXES._

trait RDFValidator {
  type Label
  type Schema 
  type Node = RDFNode
  type Result = ValidationResult[Node,Label,Throwable]

  def id: String
  
  def rdf: RDFReader
  def schema: Schema
  
  def emptyResult: Result
  
  def subjects: List[RDFNode] = rdf.subjects.toList
  def labels: Seq[Label]
  def labelStr(str: String): Label

  def match_node_label(node: Node)(label: Label): Result
  
  def match_label_node(label: Label)(node: Node): Result = {
    match_node_label(node)(label)
  }
  
  def match_node_AllLabels(node:Node): Result = {
    ValidationResult.mergeAll(labels.toList, emptyResult, match_node_label(node))
  }
  
  def matchAllNodes_Label(lbl: Label): Result = {
    ValidationResult.mergeAll(subjects, emptyResult, match_label_node(lbl))
  }

  def matchAllNodes_AllLabels: Result = {
    ValidationResult.mergeAll(subjects, emptyResult, match_node_AllLabels)
  }
  
  // Validates declared ScopeNode's only
  def validate: Result // Seq[ValidationAttempt[Node,Label]] 

}

