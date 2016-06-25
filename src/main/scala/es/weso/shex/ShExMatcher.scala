package es.weso.shex

import java.lang._

import scala.util._

import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.validator._
import es.weso.shex.PREFIXES._
import es.weso.typing._
import es.weso.utils.Logging
import es.weso.utils.Debugging

case class ShExMatcher(
    schema: Schema, 
    rdf: RDFReader
   ) extends RDFValidator with Debugging {
  
  override def id = "ShEx3" 
  type Schema = es.weso.shex.Schema
  type Label = es.weso.shex.Label
  
  override def labelStr(str: String): Label = {
    Label.labelStr(str)
  }
  
  type ValidationSchema = Schema
  type Result_ = ShExResult 

  override def emptyResult = {
    ShExResult(Success(Seq(PosNegTyping.empty)))
  }

  override def labels: Seq[Label] = {
    schema.labels
  }

  def match_nodesLabels(decls:Seq[(RDFNode,Label)]): Result_ = {
    val result = schema.matchNodesLabels(decls, rdf)
    val typing = result.map(r => r.map(_._1))
    ShExResult(typing)
  }

  def match_node_label(node:RDFNode)(label:Label): Result_ = {
    info(s"Matching $node with $label")
    val result = schema.matchNode_Label(node, label, rdf)
    val typing = result.map(r => r.map(_._1))
    info(s"Result: $typing")
    ShExResult(typing)
  }

  def validate: Result_ = {
    def triple2NodeLabel(t: RDFTriple): Try[(Node,Label)] = {
      Label.mkLabel(t.subj) match {
        case Some(label) => Success(t.obj,label)
        case None => Failure(throw new Exception(s"Node ${t.obj} cannot be a label")) 
      }
    }
    val decls = Try(rdf.triplesWithPredicate(sh_scopeNode).map(triple2NodeLabel).map(_.get).toSeq)
    decls match {
      case Success(nodesLabels) => {
        if (nodesLabels.isEmpty) {
          warn("No scopeNode declarations found")
        }
        val result = schema.matchNodesLabels(nodesLabels, rdf)
        val typing = result.map(r => r.map(_._1))
        ShExResult(typing)
      }
      case Failure(e) => 
        ShExResult.fail(e.getMessage())
    }
    
  }

  def validateAttempts: Seq[ValidationAttempt[RDFNode,Label]] = {
    val assertions = rdf.triplesWithPredicate(sh_scopeNode).map(t => (t.subj,t.obj)).toSeq
    assertions.map{ case (labelNode, node) => {
       Label.mkLabel(labelNode) match {
         case Some(label) => {
           val matcher = ShExMatcher(schema,rdf)
           val result = matcher.match_node_label(node)(label)
           ScopeNodeAttempt(node,labelNode,result)
         }
         case None => 
           ScopeNodeAttempt(node,labelNode,ShExResult.fail(s"Node $labelNode cannot be a shape label"))
       }
      }
    }

  }
} 

object ShExMatcher {
  
  def isValid(result: Try[Seq[PosNegTyping[RDFNode,Label]]]): Boolean = {
    result match {
      case Failure(_) => false
      case Success(xs) => !xs.isEmpty
    }
  }
  
}