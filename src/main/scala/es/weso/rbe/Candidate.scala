package es.weso.rbe

trait Candidate[+Edge,+Node,+Label] {
  def sign : Int
  def value: ConstraintRef
  def isPending: Boolean
}

case class Pos[Edge,Node](
    ref : ConstraintRef, 
    arc: (Node,Edge,Node)) extends Candidate[Edge,Node,Nothing] {
  def sign = 1
  def value = ref
  def isPending = false
}

case class Pending[Edge,Node,Label](
    n : ConstraintRef, 
    obj: Node, 
    ref: Label,
    arc:(Node,Edge,Node)) extends Candidate[Edge,Node,Label] {
  def sign = 1
  def value = n
  def isPending = true
}

case class Neg[Edge,Node](
    n : ConstraintRef,
    arc:(Node,Edge,Node)) extends Candidate[Edge,Node,Nothing] {
  def sign = -1
  def value = n
  def isPending = false
}
