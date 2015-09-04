package es.weso.shacl

import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes._
import util._
import Shacl._
import es.weso.rdf.PrefixMap
import es.weso.utils.PrefixMapUtils._
import org.scalactic._

trait ValidationState {
 def combineTyping(t: Typing): Try[ValidationState]
 def combine(other: ValidationState): ValidationState
 def addChecked(t: RDFTriple): Try[ValidationState] 
 def addPending(p: RDFTriple): Try[ValidationState]
 def resultLabels(node: RDFNode): Set[Label]
 def remaining: Set[RDFTriple]
 def checked: Set[RDFTriple]
 def hasRemaining = remaining.isEmpty
 def containsNodeLabel(node:RDFNode,label:Label): Boolean = {
   this match {
     case pass: Pass => pass.typing.containsType(node, label)
     case _ => false
   }
 }
 def getTyping: Try[Typing]
 
 def showState(implicit pm:PrefixMap): String
 
}


/**
 * Successful validation state
 * @param typing current typing
 * @param remaining remaining triples to check (if the shape is open, it is not an error if one of the remaining triples is not matched)
 * @param checked set of checked triples
 * @param pending set of triples that need to be validated 
 */
case class Pass(
    typing: Typing,
    remaining: Set[RDFTriple],
    checked: Set[RDFTriple],
    pending: Set[RDFTriple]
    ) extends ValidationState {
  override def getTyping: Try[Typing] = Success(typing)
  
  override def combine(other: ValidationState): ValidationState = {
    other match {
      case s2 : Pass =>
        typing.combine(s2.typing) match {
          case Success(t2) => 
            Pass(typing = t2,
             checked = checked ++ s2.checked,
             remaining = remaining -- s2.remaining,
             pending = pending ++ s2.pending
            )
          case Failure(e) => throw new Exception("combine: failure combining typings " + e)
        }
      case f : Fail => f
    }
  }

  override def combineTyping(other: Typing): Try[ValidationState] = {
    typing combine other map { 
      t => this.copy(typing = t)
    }
  }
  
  override def addChecked(t: RDFTriple): Try[ValidationState] = {
    Success(this.copy(checked = checked + t, remaining = remaining - t))
  }

  override def addPending(t: RDFTriple): Try[ValidationState] = {
    Success(this.copy(pending = pending + t))
  }
  
 override def resultLabels(node: RDFNode): Set[Label] = {
   typing.hasShapes(node)
 }
 
 override def showState(implicit pm: PrefixMap): String = {
   "Pass(typing: " + typing.showTyping(pm) + 
   ", checked: " + showTriples(checked)(pm) + 
   ", remaining: " + showTriples(remaining)(pm) + 
   ", pending: " + showTriples(remaining)(pm) + 
   ")"
 }
  
}
    
case class Fail(
    reasons: List[ValidationError]
   ) extends ValidationState {
  
  override def getTyping: Try[Typing] = Failure(new Exception("Cannot get typing of failed state: " + this))
  
  override def checked: Set[RDFTriple] = ???
  override def remaining: Set[RDFTriple] = ???

  // TODO: Review this combination (it ignores the other state)
  override def combine(other: ValidationState): ValidationState = {
    other match {
      case _:Pass => this
      case Fail(rs) => this.copy(reasons = reasons ++ rs)
    }
  }

  override def combineTyping(other: Typing): Try[ValidationState] = {
    Failure(new Exception("Cannot combine typing: " + other + ", with state: " + this))
  }

  override def addChecked(t: RDFTriple): Try[ValidationState] = {
    Failure(new Exception("Cannot add checked triple: " + t + " to failed state: " + this))
  }
  
  override def addPending(t: RDFTriple): Try[ValidationState] = {
    Failure(new Exception("Cannot add pending triple: " + t + " to failed state: " + this))
  }
  
  override def resultLabels(node: RDFNode): Set[Label] = {
   // TODO: throw an exception?
   Set()
 }
  
 override def showState(implicit pm: PrefixMap): String = {
   "Fail(" ++ reasons.map(_.show(pm)).mkString(",") ++ ")"
 }

}

object ValidationState {

  def empty : ValidationState = {
    Pass(Typing.emptyTyping, Set(), Set(),Set())
  }
 
}