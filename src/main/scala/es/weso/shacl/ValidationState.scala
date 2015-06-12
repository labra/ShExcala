package es.weso.shacl

import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes._
import es.weso.monads.Result._
import es.weso.monads.Result
import util._
import Shacl._

sealed trait ValidationState {
 def combineTyping(t: Typing): Result[ValidationState]
 def combine(other: ValidationState): ValidationState
 def addChecked(t: RDFTriple): Result[ValidationState] 
 def resultLabels(node: RDFNode): Set[Label]
 def remaining: Set[RDFTriple]
 def checked: Set[RDFTriple]
 def hasRemaining = remaining.isEmpty
}

case class Pass(
    typing: Typing,
    remaining: Set[RDFTriple],
    checked: Set[RDFTriple]
    ) extends ValidationState {
  
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
      case f : Failed => f
    }
  }

  override def combineTyping(other: Typing): Result[ValidationState] = {
    val combinedTyping = typing combine other
    combinedTyping match {
      case Success(t) => unit(this.copy(typing = t))
      case Failure(e) => failure("Cannot combine typings" + e.getMessage)
    }
  }
  
  override def addChecked(t: RDFTriple): Result[ValidationState] = {
    unit(this.copy(checked = checked + t))
  }

 override def resultLabels(node: RDFNode): Set[Label] = {
   typing.hasShapes(node)
 }
  
}
    
case class Failed(
    checked: Set[RDFTriple],
    remaining: Set[RDFTriple],
    failed: Set[(RDFTriple,RDFNode,ShapeExpr)]
) extends ValidationState {

  // TODO: Review this combination (it ignores the other state)
  override def combine(other: ValidationState): ValidationState = {
    this
  }

  override def combineTyping(other: Typing): Result[ValidationState] = {
    failure("Cannot combine typing: " + other + ", with state: " + this)
  }

  override def addChecked(t: RDFTriple): Result[ValidationState] = {
    failure("Cannot add checked triple: " + t + ", with state: " + this)
  }
  
  override def resultLabels(node: RDFNode): Set[Label] = {
   // TODO: throw an exception?
   Set()
 }

}

object ValidationState {

  def empty : ValidationState = {
    Pass(Typing.emptyTyping, Set(), Set())
  }
 
}