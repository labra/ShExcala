package es.weso.rbe

trait Candidate[+Edge,+Node,+Label,+Err] {
  def sign : Int
  def value: ConstraintRef
  def isPending: Boolean
  def edge: Edge
}

case class Pos[Edge,Node](
    ref : ConstraintRef, 
    arc: (Node,Edge,Node)) extends Candidate[Edge,Node,Nothing,Nothing] {
  override def edge = arc._2
  def sign = 1
  def value = ref
  def isPending = false
}

case class Pending[Edge,Node,Label](
    n : ConstraintRef, 
    obj: Node, 
    ref: Label,
    arc:(Node,Edge,Node)) extends Candidate[Edge,Node,Label,Nothing] {
  override def edge = arc._2
  def sign = 1
  def value = n
  def isPending = true
}

case class Neg[Edge,Node,Err](
    n : ConstraintRef,
    arc:(Node,Edge,Node),
    errors: Seq[Err]) extends Candidate[Edge,Node,Nothing,Err] {
  override def edge = arc._2
  def sign = -1
  def value = n
  def isPending = false
}

case class Missing[Edge,Node,Err](
   n: ConstraintRef, 
   arc: (Node,Edge,Node)
   ) extends Candidate[Edge,Node,Nothing,Err] {
  override def edge = arc._2
  def sign = -1
  def value = n
  def isPending = false
}
