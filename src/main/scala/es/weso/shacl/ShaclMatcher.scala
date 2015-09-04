package es.weso.shacl

import es.weso.rdf._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import java.lang._
import es.weso.utils.Logging
import util._
import es.weso.typing._

case class ShaclMatcher(
    schema: Schema, 
    rdf: RDFReader
   ) extends {
  
  lazy val id = "ShEx3" 
  
  type Node = RDFNode
  type Label = Shacl.Label
  type ValidationSchema = Schema
  type Result_ = Try[Seq[PosNegTyping[Node,Label]]]

  def match_node_label(node:RDFNode)(label:Label): Result_ = {
    schema.matchNode_Label(node, label, rdf)
  }
  
} 

object ShaclMatcher {
  
  def isValid(result: Try[Seq[PosNegTyping[RDFNode,Label]]]): Boolean = {
    result match {
      case Failure(_) => false
      case Success(xs) => !xs.isEmpty
    }
  }
}