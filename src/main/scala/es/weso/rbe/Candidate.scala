package es.weso.rbe

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
