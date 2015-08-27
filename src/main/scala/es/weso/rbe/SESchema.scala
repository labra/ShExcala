package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._
import Graph._
import util.{Try,Success,Failure}

case class SESchemaException(msg: String) extends Exception(s"SESchema: $msg")

sealed trait NodeShape[+Label,+Node]
case class Ref[Label](label:Label) extends NodeShape[Label,Nothing]
case class Pred[Node](pred:Node => Boolean) extends NodeShape[Nothing,Node]

case class Table[Edge,Node,Label](
    constraints: Map[Int,NodeShape[Label,Node]],
    edges: Map[Edge,Set[Int]],
    elems: Int) {
  def addEdge(e: Edge, n: Int): Map[Edge,Set[Int]] = {
    edges.updated(e, 
        edges.get(e).getOrElse(Set()) + n)
  }
}

object Table {
  def empty[Edge,Node,Label]: Table[Edge,Node,Label] =
    Table(Map(),Map(),0)
}    

case class SESchema[Edge,Node,Label](m: Map[Label,Sorbe[(Edge,NodeShape[Label,Node])]]) {
  type ReasonPos = Nothing
  type ReasonNeg = Nothing
  type Constraint = Int
  type RBE_ = Sorbe[(Edge,NodeShape[Label,Node])] 
  type Table_ = Table[Edge,Node,Label]
    
  def mkTable(label: Label): Try[(Table_, Sorbe[Int])] =
    if (m contains label)
     Success(mkTableAux(m(label),Table.empty))
    else 
      Failure(SESchemaException(s"Schema does not contain label $label. Table: $m"))
     
  private def mkTableAux(
      rbe: RBE_,
      current: Table_): (Table_,Sorbe[Int]) = {
    rbe match {
      case Empty => (current, Empty)
      case Symbol((p,c),m,n) => {
        val newElem = current.elems + 1
        val newTable = current.copy(
          elems = newElem,
          constraints = current.constraints + (newElem -> c),
          edges = current.addEdge(p,newElem)
         )
         (newTable,Symbol(newElem,m,n))
      }
       case And(s1,s2) => {
         val (t1,r1) = mkTableAux(s1,current)
         val (t2,r2) = mkTableAux(s2,t1)
         (t2, And(r1,r2))
       }
       case Or(s1,s2) => {
         val (t1,r1) = mkTableAux(s1,current)
         val (t2,r2) = mkTableAux(s2,t1)
         (t2, Or(r1,r2))
       }
       case Plus(s) => {
         val (t,r) = mkTableAux(s,current)
         (t, Plus(r))
       }
       case Star(s) => {
         val (t,r) = mkTableAux(s,current)
         (t, Star(r))
       }
    }
  }

  def matchNode(
      n: Node, 
      label: Label, 
      graph: Graph[Edge,Node]): Seq[PosNegTyping[Node,Label]] = {
    val out = graph.out(n)
    val rbe = m(label)
    val t : PosNegTyping[Node,Label] = PosNegTyping.empty
    Seq(t)
  }
  
}
