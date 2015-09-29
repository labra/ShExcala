package es.weso.rdf.validator

import es.weso.rdf._;
import es.weso.rdfgraph.nodes._
import es.weso.monads.Result
import es.weso.shex.Typing
import util._
import es.weso.rdf.PrefixMap

trait RDFValidator {
  type Label
  type Schema 
  type Node = RDFNode

  def id: String
  
  def rdf: RDFReader
  def schema: Schema
  def emptyResult: ValidationResult[Node,Label,Throwable]
  
  def subjects: List[RDFNode] = rdf.subjects.toList
  def labels: Seq[Label]
  def mkLabel(str: String): Label

  def match_node_label(node: Node)(label: Label): ValidationResult[Node,Label,Throwable]
  
  def match_label_node(label: Label)(node: Node): ValidationResult[Node,Label,Throwable] = {
    match_node_label(node)(label)
  }
  
  def match_node_AllLabels(node:Node): ValidationResult[Node,Label,Throwable] = {
    ValidationResult.mergeAll(labels.toList, emptyResult, match_node_label(node))
  }
  
  def matchAllNodes_Label(lbl: Label): ValidationResult[Node,Label,Throwable] = {
    ValidationResult.mergeAll(subjects, emptyResult, match_label_node(lbl))
  }

  def matchAllNodes_AllLabels: ValidationResult[Node,Label,Throwable] = {
    ValidationResult.mergeAll(subjects, emptyResult, match_node_AllLabels)
  }

}

