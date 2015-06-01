package es.weso.shacl

import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.monads.Result._
import es.weso.monads.Result
import util._
import Shacl._

sealed trait ValidationState {
 def combineTyping(t: Typing): Result[ValidationState]
 def addChecked(t: RDFTriple): Result[ValidationState] 
}

case class Pass(
    typing: Typing,
    remaining: Set[RDFTriple],
    checked: Set[RDFTriple]
    ) extends ValidationState {

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

}
    
case class Failed(
    checked: Set[RDFTriple],
    remaining: Set[RDFTriple],
    failed: Set[(RDFTriple,RDFNode,ShapeExpr)]
) extends ValidationState {

  override def combineTyping(other: Typing): Result[ValidationState] = {
    failure("Cannot combine typing: " + other + ", with state: " + this)
  }

  override def addChecked(t: RDFTriple): Result[ValidationState] = {
    failure("Cannot add checked triple: " + t + ", with state: " + this)
  }
}

object ValidationState {

  def empty : ValidationState = {
    Pass(Typing.emptyTyping, Set(), Set())
  }
 
}