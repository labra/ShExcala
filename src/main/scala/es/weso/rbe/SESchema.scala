package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._
import Graph._
import util.{Try,Success,Failure}
import es.weso.utils.SeqUtils._
import es.weso.utils.TryUtils._

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

case class Shape[Edge,Node,Label](
    rbe: Sorbe[(Edge,NodeShape[Label,Node])], 
    extras: Seq[Edge], 
    closed: Boolean
)

object Shape {
  def empty = Shape(rbe = Empty, extras = Seq(), closed = false)
}

case class Schema[Edge,Node,Label](m: Map[Label,Shape[Edge,Node,Label]]) {
  type ReasonPos = Nothing
  type ReasonNeg = Nothing
  type Constraint = Int
  
  // These types are specialized versions of the general types for readability
  type RBE_ = Sorbe[(Edge,NodeShape[Label,Node])] 
  type Table_ = Table[Edge,Node,Label]
  type OutNeigh_ = Seq[(Edge,Node)]
  type Candidate_ = Candidate[Node,Label]
  type Candidates_ = Seq[Option[Candidate_]]
  type Typing_ = PosNegTyping[Node,Label]
  type Result_ = Try[Seq[Typing_]]
  type Graph_ = Graph[Edge,Node]
  type Schema_ = Schema[Edge,Node,Label]
    
  def mkTable(label: Label): Try[(Table_, Sorbe[ConstraintRef])] =
    if (m contains label)
     Success(mkTableAux(m(label).rbe,Table.empty))
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
    val pos = defined.filter(_.get.sign == 1).map(_.get.value)
    val neg = defined.filter(_.get.sign == -1).map(_.get.value)
    pos.intersect(neg).length != 0
  }
  
  // TODO: It ignores extra predicates (value None) and negative candidates by now
  def candidatesToBag(cs: Seq[Option[Candidate_]]): Bag[ConstraintRef] = {
    Bag.toBag(cs.filter(x => x.isDefined && x.get.sign == 1).map(_.get.value))
  }

  def zipCandidates(table: Table_, out: Seq[(Edge,Node)]): Seq[Seq[Option[Candidate_]]] = {
    zipN(candidates(table,out))
  }
  
  def calculateCandidates(
      table: Table_, 
      out: OutNeigh_, 
      sorbe: Sorbe[ConstraintRef]): Try[Seq[Candidates_]] = {
    Try{
     filterCandidates(table,out,sorbe) 
    }
  }
  
  def combineTypings(t1: Typing_, t2: Typing_): Try[Typing_] = {
    t1.combine(t2)
  }
  
  def combineResults(r1: Result_, r2: Result_): Result_ = {
    ??? // r1 r2
  }
  
  def resolveCandidate(g: Graph_,s:Schema_)(
      rest: Result_, 
      c: Option[Candidate_]): Result_ = {
    c match {
      case None => rest
      case Some(Pending(c,node,label)) => rest match {
        case Failure(e) => Failure(e)
        case Success(currentTypings) => {
          val rs = currentTypings.map(t => matchNode(node,label,g,s,t))
          val f = filterSuccess(rs)
          f.map(t => t.flatten)
        } 
      }
      case Some(_) => rest
    }
  }
  
  def resolveCandidates(
      cs: Candidates_, 
      node: Node, 
      label: Label,
      g: Graph_,
      s: Schema_,
      t: Typing_): Result_ = {
    val zero : Result_ = t.addPosType(node, label).map(Seq(_)) 
    cs.foldLeft(zero)(resolveCandidate(g,s))
  }
  
  def filterSuccessSeq[A](ts: Seq[Try[Seq[A]]]): Try[Seq[Seq[A]]] = {
    filterSuccess(ts)
  }
  
  def filterSuccessSeqFlatten[A](ts: Seq[Try[Seq[A]]]): Try[Seq[A]] = {
    filterSuccessSeq(ts).map(s => s.flatten)
  }
  
  def resolveAllCandidates(
      css: Seq[Candidates_],
      node: Node, 
      label: Label,
      g: Graph_,
      s: Schema_,
      currentTyping: Typing_): Result_ = {
  val attempts : Seq[Result_] = css.map(cs => resolveCandidates(cs,node,label,g,s,currentTyping))
  filterSuccessSeqFlatten(attempts)
  }
  
  def matchNode(
      node: Node, 
      label: Label, 
      graph: Graph_,
      schema: Schema_,
      currentTyping: Typing_): Result_ = {
    println(s"Trying to match node $node with label $label, currentTyping: $currentTyping" )

    // If the node has already been checked, return without checking again to avoid recursion
    // TODO: Maybe, we could try again in a more dynamic setting
    if (currentTyping.getLabels(node) contains label)
      Success(Seq(currentTyping))
    else {
    val out = graph.out(node)
    for {
      (table,sorbe) <- mkTable(label)
      allCandidates <- calculateCandidates(table,out,sorbe)
      newTyping <- currentTyping.addPosType(node,label)
      typings <- resolveAllCandidates(allCandidates, node, label, graph, schema, newTyping)
    } yield {
      typings
    }
  }
  }
}

}