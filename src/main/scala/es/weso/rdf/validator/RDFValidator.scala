package es.weso.rdf.validator

import es.weso.rdf._;
import es.weso.rdfgraph.nodes._
import es.weso.monads.Result
import es.weso.shex.Typing

trait RDFValidator {
  type Label
  type Schema 
  type Node = RDFNode

  def id: String
  
  val rdf: RDFReader
  val schema: Schema
  val emptyResult: ValidationResult[Node,Label]
  
  def subjects: List[RDFNode] = rdf.subjects.toList
  def labels: Seq[Label]

  def match_node_label(node: Node)(label: Label): ValidationResult[Node,Label]
  
  def match_label_node(label: Label)(node: Node): ValidationResult[Node,Label] = {
    match_node_label(node)(label)
  }
  
  def match_node_AllLabels(node:Node): ValidationResult[Node,Label] = {
    ValidationResult.passSome(labels.toList, emptyResult, match_node_label(node))
  }
  
  def matchAllNodes_Label(lbl: Label): ValidationResult[Node,Label] = {
    ValidationResult.combineAll(subjects, emptyResult, match_label_node(lbl))
  }

  def matchAllNodes_AllLabels: ValidationResult[Node,Label] = {
    ValidationResult.combineAll(subjects, emptyResult, match_node_AllLabels)
  }
 
}


