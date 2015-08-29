package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._
import Graph._
import util.{Try,Success,Failure}
import es.weso.utils.SeqUtils._

package object SESchema {

case class SESchemaException(msg: String) extends Exception(s"SESchema: $msg")

sealed trait NodeShape[+Label,+Node]

// Reference to another label
case class Ref[Label](label:Label) extends NodeShape[Label,Nothing]

// Boolean Constraint on nodes (it has a name and a predicate)
case class Pred[Node](
    name: String, 
    pred:Node => Boolean) extends NodeShape[Nothing,Node]

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

case class Table[Edge,Node,Label](
    constraints: Map[ConstraintRef,NodeShape[Label,Node]],
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
  def empty[Edge,Node,Label]: Table[Edge,Node,Label] =
    Table(Map(),Map(),0)
}    

case class Schema[Edge,Node,Label](m: Map[Label,Sorbe[(Edge,NodeShape[Label,Node])]]) {
  type ReasonPos = Nothing
  type ReasonNeg = Nothing
  type Constraint = Int
  type RBE_ = Sorbe[(Edge,NodeShape[Label,Node])] 
  type Table_ = Table[Edge,Node,Label]
  type OutNeigh_ = Seq[(Edge,Node)]
  type Candidate_ = Candidate[Node,Label]
  type Candidates_ = Seq[Option[Candidate_]]
    
  def mkTable(label: Label): Try[(Table_, Sorbe[ConstraintRef])] =
    if (m contains label)
     Success(mkTableAux(m(label),Table.empty))
    else 
      Failure(SESchemaException(s"Schema does not contain label $label. Table: $m"))
     
  private def mkTableAux(
      rbe: RBE_,
      current: Table_): (Table_,Sorbe[ConstraintRef]) = {
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
  
  def testCandidate(shape: NodeShape[Label,Node], node: Node, c: ConstraintRef): Candidate_ = {
    shape match {
      case Ref(label) => Pending(c,node,label)  // Possible optimization if Ref has already been checked
      case p: Pred[Node] => 
        if (p.pred(node)) Pos(c)
        else Neg(c)
      case _ => throw SESchemaException(s"testCandidate: unknown shape $shape")
    }
  }
  
  def lookupEdgeConstraints(table: Table_, edge: Edge): Seq[ConstraintRef] = {
    table.edges.get(edge).getOrElse(Set()).toSeq
  }
  
  def lookupConstraintShape(table: Table_, c: ConstraintRef): NodeShape[Label,Node] = {
    table.constraints.get(c) match {
      case None => throw SESchemaException(s"Cannot find constraintRef $c in table $table")
      case Some(ns) => ns
    }
  }
  
  def possibleCandidates(table: Table_, pair: (Edge,Node)): Seq[Candidate_] = {
    val (edge,node) = pair
    lookupEdgeConstraints(table, edge).map(c => 
      testCandidate(lookupConstraintShape(table, c),node,c)
    )
  }
  
  def candidates(table: Table_, out: OutNeigh_): Seq[Seq[Candidate_]] = {
    out.map(pair => possibleCandidates(table,pair))
  }
  
  def filterCandidates(
      table: Table_, 
      out: OutNeigh_, 
      sorbe: Sorbe[ConstraintRef]): Seq[Seq[Option[Candidate_]]] = {
    val css = zipCandidates(table,out)
    css.filter(cs => checkCandidate(cs,sorbe))
  }
  
  def pending(cs: Seq[Option[Candidate_]]): Seq[(Node,Label)] = {
    cs.filter(c => c.isDefined && c.get.isPending).map{ case c => c.get match {
      case Pending(c,n,l) => (n,l)
      case _ => throw SESchemaException(s"pending: Unexpected value: $c")
    }}
  }
  
  def pendings(css: Seq[Candidates_]): Seq[(Node,Label)] = {
    css.map(cs => pending(cs)).flatten
  }
  
  def matchNoPending(table: Table_, out: OutNeigh_, sorbe: Sorbe[ConstraintRef]): Boolean = {
    val css = filterCandidates(table, out, sorbe)
    !css.isEmpty && pendings(css).isEmpty
  }
  
  def checkCandidate(cs: Seq[Option[Candidate_]], sorbe: Sorbe[ConstraintRef]): Boolean = {
    println(s"Checking candidate: $cs with sorbe: $sorbe")
    val b = !containsContradictions(cs) && sorbe.contains(candidatesToBag(cs))
    println(s"Result: $b")
    b
  }
  
  // TODO: The following code could be optimized using some mathematical formula
  // A contradiction appears when a value N has sign -1 and another value N has sign +1
  def containsContradictions(cs: Seq[Option[Candidate_]]): Boolean = {
    val defined = cs.filter(_.isDefined)
    val pos = defined.filter(_.get.sign == 1)
    val neg = defined.filter(_.get.sign == -1)
    pos.intersect(neg).length != 0
  }
  
  // TODO: It ignores extra predicates (value None) and negative candidates by now
  def candidatesToBag(cs: Seq[Option[Candidate_]]): Bag[ConstraintRef] = {
    Bag.toBag(cs.filter(x => x.isDefined && x.get.sign == 1).map(_.get.value))
  }

  def zipCandidates(table: Table_, out: Seq[(Edge,Node)]): Seq[Seq[Option[Candidate_]]] = {
    zipN(candidates(table,out))
  }
  
  def calculateCandidates(table: Table_, out: OutNeigh_, sorbe: Sorbe[ConstraintRef]): Try[Seq[Candidates_]] = {
    Try{
     filterCandidates(table,out,sorbe) 
    }
  }
  
  def resolveCandidates(cs: Candidates_, 
      node: Node, 
      label: Label, 
      t: PosNegTyping[Node,Label]):Try[PosNegTyping[Node,Label]] = {
    if (pending(cs).isEmpty) t.addPosType(node, label)
    else t.addNegType(node, label)
  }
  
  def flattenTrys[A](xs: Seq[Try[A]]): Try[Seq[A]] = 
    ???
  
  def resolveAllCandidates(css: Seq[Candidates_],
      node: Node, 
      label: Label,
      t: PosNegTyping[Node,Label]): Try[Seq[PosNegTyping[Node,Label]]] = {
    val x = css.map(cs => resolveCandidates(cs,node,label,t))
    flattenTrys(x)
  }
  
  def matchNode(
      node: Node, 
      label: Label, 
      graph: Graph[Edge,Node]): Try[Seq[PosNegTyping[Node,Label]]] = {
    val out = graph.out(node)
    val t : PosNegTyping[Node,Label] = PosNegTyping.empty
    for {
      (table,sorbe) <- mkTable(label)
      allCandidates <- calculateCandidates(table,out,sorbe)
      typings <- resolveAllCandidates(allCandidates, node, label, t)
    } yield {
      typings
    }
  }
  
  
}

}