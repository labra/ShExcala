package es.weso.shacl

import es.weso.rdf._
import es.weso.rdfgraph.nodes._
import java.lang._
import es.weso.utils.Logging
import util._
import es.weso.typing._
import es.weso.rdf.validator._

case class ShaclMatcher(
    schema: Schema, 
    rdf: RDFReader
   ) extends RDFValidator {
  
  override def id = "ShEx3" 
  type Schema = es.weso.shacl.Schema
  type Label = es.weso.shacl.Label
  
  override def mkLabel(str: String): Label = {
    Label.mkLabel(str)
  }
  type ValidationSchema = Schema
  type Result_ = ShaclResult 

  override def emptyResult = {
    ShaclResult(Success(Seq(PosNegTyping.empty)))
  }

  override def labels: Seq[Label] = {
    schema.labels
  }

  def match_node_label(node:RDFNode)(label:Label): Result_ = {
    val result = schema.matchNode_Label(node, label, rdf)
    val typing = result.map(r => r.map(_._1))
//    ShaclResult(result.map(rs => rs.map(pairs => pairs._1)))
    ShaclResult(typing)
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