package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._
import Graph._
import util.{Try,Success,Failure}
import es.weso.utils.SeqUtils._
import es.weso.utils.TryUtils._
import es.weso.utils.Checker

package object SESchema {

case class SESchemaException(msg: String) extends Exception(s"SESchema: $msg")


type ConstraintRef = Int 
trait Candidate[+Node,+Label] {
  def sign : Int
  def value: ConstraintRef
  def isPending: Boolean
}
case class Pos(n : ConstraintRef) extends Candidate[Nothing,Nothing] {
  def sign = 1
  def value = n
  def isPending = false
}
case class Pending[Node,Label](n : ConstraintRef, node: Node, ref: Label) extends Candidate[Node,Label] {
  def sign = 1
  def value = n
  def isPending = true
}
case class Neg(n : ConstraintRef) extends Candidate[Nothing,Nothing] {
  def sign = -1
  def value = n
  def isPending = false
}

case class Table[Edge,Node,Label,Err](
    constraints: Map[ConstraintRef,NodeShape[Label,Node,Err]],
    edges: Map[Edge,Set[ConstraintRef]],
    elems: Int) {
  def addEdge(
      e: Edge, 
      n: ConstraintRef): Map[Edge,Set[ConstraintRef]] = {
    edges.updated(e, 
        edges.get(e).getOrElse(Set()) + n)
  }
}

object Table {
  def empty[Edge,Node,Label,Err]: Table[Edge,Node,Label,Err] =
    Table(Map(),Map(),0)
}    

case class Shape[Edge,Node,Label,Err](
    rbe: Sorbe[(Edge,NodeShape[Label,Node,Err])], 
    extras: Seq[Edge], 
    closed: Boolean
)

object Shape {
  def empty = Shape(rbe = Empty, extras = Seq(), closed = false)
}

}