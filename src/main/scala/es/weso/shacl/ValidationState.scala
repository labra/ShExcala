package es.weso.shacl

import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes._
import util._
import Shacl._

sealed trait ValidationState {
 def combineTyping(t: Typing): Try[ValidationState]
 def combine(other: ValidationState): ValidationState
 def addChecked(t: RDFTriple): Try[ValidationState] 
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
}

case class Pass(
    typing: Typing,
    remaining: Set[RDFTriple],
    checked: Set[RDFTriple]
    ) extends ValidationState {
  override def getTyping: Try[Typing] = Success(typing)
  
  override def combine(other: ValidationState): ValidationState = {
    other match {
      case s2 : Pass =>
        typing.combine(s2.typing) match {
          case Success(t2) => 
            Pass(typing = t2,
             checked = checked ++ s2.checked,
             remaining = remaining -- s2.remaining 
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

 override def resultLabels(node: RDFNode): Set[Label] = {
   typing.hasShapes(node)
 }
 
 override def toString: String = {
    "Pass state: typing: " + typing + 
    "\nchecked: " + checked + 
    "\nremaining: " + remaining
 }
  
}
    
case class Fail(
    checked: Set[RDFTriple],
    remaining: Set[RDFTriple],
    failed: Set[(RDFTriple,RDFNode,ShapeExpr)]
) extends ValidationState {
  
  override def getTyping: Try[Typing] = Failure(new Exception("Cannot get typing of failed state: " + this))

  // TODO: Review this combination (it ignores the other state)
  override def combine(other: ValidationState): ValidationState = {
    this
  }

  override def combineTyping(other: Typing): Try[ValidationState] = {
    Failure(new Exception("Cannot combine typing: " + other + ", with state: " + this))
  }

  override def addChecked(t: RDFTriple): Try[ValidationState] = {
    Failure(new Exception("Cannot add checked triple: " + t + ", with state: " + this))
  }
  
  override def resultLabels(node: RDFNode): Set[Label] = {
   // TODO: throw an exception?
   Set()
 }
  
  override def toString: String = 
    "Fail state: checked = " + checked + 
    "\nRemaining = " + remaining + 
    "\nFailed: " + failed

}

object ValidationState {

  def empty : ValidationState = {
    Pass(Typing.emptyTyping, Set(), Set())
  }
 
}