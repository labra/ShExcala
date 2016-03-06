package es.weso.rbe

/**
 * A candidate to match
 */
trait Candidate[Edge,+Node,+Label,+Err] {
  def sign : Int
  def value: ConstraintRef
  def isPending: Boolean
  def edge: DirectedEdge[Edge]
}

/**
 * A positive candidate
 */
case class Pos[Edge,Node](
    ref : ConstraintRef, 
    arc: (Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Nothing,Nothing] {
  def sign = 1
  def value = ref
  def isPending = false
}

/**
 * A candidates that is pending 
 */
case class Pending[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Label,
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label,Nothing] {
  def sign = 1
  def value = n
  def isPending = true
}

case class PendingNot[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Label,
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label,Nothing] {
  def sign = 1
  def value = n
  def isPending = true
}

case class PendingSeq[Edge,Node,Label](
    n : ConstraintRef, 
    node: Node, 
    ref: Seq[Label],
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge]) extends Candidate[Edge,Node,Label,Nothing] {
  def sign = 1
  def value = n
  def isPending = true
}


/**
 * A negative candidate
 */
case class Neg[Edge,Node,Err](
    n : ConstraintRef,
    arc:(Node,Edge,Node),
    edge: DirectedEdge[Edge],
    errors: Seq[Err]) extends Candidate[Edge,Node,Nothing,Err] {
  def sign = -1
  def value = n
  def isPending = false
}

/**
 * A missing candidate
 */
case class Missing[Edge,Node,Err](
   n: ConstraintRef, 
   arc: (Node,Edge,Node),
   edge: DirectedEdge[Edge]   
   ) extends Candidate[Edge,Node,Nothing,Err] {
  def sign = -1
  def value = n
  def isPending = false
}
