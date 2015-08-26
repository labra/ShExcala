package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._
import Graph._

sealed trait NodeShape[+Label,+Node]
case class Ref[Label](label:Label) extends NodeShape[Label,Nothing]
case class Pred[Node](pred:Node => Boolean) extends NodeShape[Nothing,Node]


case class SESchema[Edge,Node,Label](m: Map[Label,Sorbe[(Edge,NodeShape[Label,Node])]]) {
  type ReasonPos = Nothing
  type ReasonNeg = Nothing

//  def candidates(cs: Seq[(Edge,Node)], rbe: Sorbe[(Edge,NodeShape[Label,Node])]): Seq[()]

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
