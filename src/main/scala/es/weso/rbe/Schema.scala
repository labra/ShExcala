package es.weso.rbe

import es.weso.rbe._
import util._
import es.weso.utils.Checker
import es.weso.collection.Bag
import es.weso.utils.SeqUtils._
import es.weso.utils.TryUtils._
import es.weso.typing.PosNegTyping
import es.weso.utils.Logging

case class Schema[Edge,Node,Label,Err](m: Map[Label,Shape[Edge,Node,Label,Err]]) extends Logging {
  type ReasonPos = Nothing
  type ReasonNeg = Nothing
  type Constraint = ConstraintRef
  
  // These types are specialized versions of the general types for readability
  type RBE_ = Sorbe[(Edge,NodeShape[Label,Node,Err])] 
  type Table_ = Table[Edge,Node,Label,Err]
  type Arc_ = (Node,Edge,Node)
  type OutNeigh_ = Seq[(Edge,Node)]
  type Candidate_ = Candidate[Edge,Node,Label]
  type Candidates_ = Seq[Option[Candidate_]]
  type Typing_ = PosNegTyping[Node,Label]
  type SingleResult_ = (Typing_,Seq[(Node,Edge,Node)])
  type Result_ = Try[Seq[SingleResult_]]
  type Graph_ = Graph[Edge,Node]
  type Schema_ = Schema[Edge,Node,Label,Err]
  type Check_ = Checker[Err,Node]
    
  def mkTable(label: Label): Try[(Table_, Sorbe[ConstraintRef])] = {
    if (m contains label) {
     val shape = m(label)
     val (table,rbe) = mkTableAux(shape.rbe,Table.empty)
     val (table1,rbe1) = addExtras(shape.extras,table,rbe)
     println(s"Table created: table = $table1, rbe = $rbe1")
     Success((table1,rbe1)) 
    }
    else 
     Failure(SESchemaException(s"Schema does not contain label $label. Table: $m"))
  }
  
  def addExtras(extras: Seq[Edge], table: Table_, rbe: Sorbe[ConstraintRef]): (Table_,Sorbe[ConstraintRef]) = {
    extras.foldLeft((table,rbe))(addExtra)
  }
  
  def addExtra(pair: (Table_, Sorbe[ConstraintRef]), extra: Edge): (Table_,Sorbe[ConstraintRef]) = {
    val (table,rbe) = pair
    val elems1 = table.elems + 1
    val ref1 = ConstraintRef(elems1)
    val dot : NodeShape[Label,Node,Err] = NodeShape.any
    val constraints1 = table.constraints + (ref1 -> dot)
    val edges1 = table.edges.updated(extra,
                  table.edges.get(extra).getOrElse(Set()) + ref1)
    val table1 = table.copy(
      elems = elems1,
      constraints = constraints1,
      edges = edges1
    )
    val rbe1 = And(rbe,Symbol(ref1,0,Unbounded)) 
    (table1,rbe1)
  }
     
  private def mkTableAux(
      rbe: RBE_,
      current: Table_): (Table_,Sorbe[ConstraintRef]) = {
    rbe match {
      case Empty => (current, Empty)
      case Symbol((p,c),m,n) => {
        val newElem = current.elems + 1
        val cref = ConstraintRef(newElem)
        val newTable = current.copy(
          elems = newElem,
          constraints = current.constraints + (cref -> c),
          edges = current.addEdge(p,cref)
         )
         (newTable,Symbol(cref,m,n))
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
       case Repeat(s,n,m) => {
         val (t,r) = mkTableAux(s,current)
         (t, Repeat(r,n,m))
       }
       case _ => throw SESchemaException(s"mkTableAux: Unsupported rbe: $rbe")
    }
  }
  
  private def checkCandidate(
      shape: NodeShape[Label,Node,Err], node: Node, edge: Edge, obj: Node, c: ConstraintRef): Candidate_ = {
        println(s"Check candidate $c with shape $shape. Edge: $edge, node: $node")
    shape match {
      case Ref(label) => 
        Pending(c,obj,label,(node,edge,obj))  // Possible optimization if Ref has already been checked
      case p: Pred[Node,Err] => {
        println(s"Checking condition with obj $obj and predicate ${p.name}")
        p.pred(obj).fold(
            (x: Node) => {
                 println(s"Condition satisfied with node $x")
             Pos(c,(node,edge,obj))
            },
            (y:Seq[Err]) => {
             println(s"Condition failed on $obj with predicate ${p.name}. Error = $y")
             Neg(c,(node,edge,obj))  // TODO: Check how to integrate error messages 
            }
        )
      } 
      case _ => throw SESchemaException(s"testCandidate: unknown shape $shape")
    }
  }
  
  private def lookupEdgeConstraints(table: Table_, edge: Edge): Seq[ConstraintRef] = {
    table.edges.get(edge).getOrElse(Set()).toSeq
  }
  
  private def lookupConstraintShape(table: Table_, c: ConstraintRef): NodeShape[Label,Node,Err] = {
    table.constraints.get(c) match {
      case None => throw SESchemaException(s"Cannot find constraintRef $c in table $table")
      case Some(ns) => ns
    }
  }
  
  def possibleCandidates(table: Table_, node: Node, pair: (Edge,Node)): Seq[Candidate_] = {
    val (edge,obj) = pair
    lookupEdgeConstraints(table, edge).map(c => 
      checkCandidate(lookupConstraintShape(table, c),node,edge,obj,c)
    )
  }
  
  def candidates(table: Table_, node: Node, out: OutNeigh_): Seq[Seq[Candidate_]] = {
    out.map(pair => possibleCandidates(table,node,pair))
  }
  
  def filterCandidates(
      table: Table_, 
      out: OutNeigh_,
      node: Node,
      sorbe: Sorbe[ConstraintRef],
      open:Boolean): Seq[Seq[Option[Candidate_]]] = {
    val css = zipCandidates(table,node,out)
    css.filter(cs => matchCandidateSorbe(cs,sorbe,open))
  }
  
  private def pending(cs: Seq[Option[Candidate_]]): Seq[(Node,Label)] = {
    cs.filter(c => c.isDefined && c.get.isPending).
       map{ case c => c.get match {
        case Pending(c,n,l,arc) => (n,l)
        case _ => throw SESchemaException(s"pending: Unexpected value: $c")
       }}
  }
  
  private def pendings(css: Seq[Candidates_]): Seq[(Node,Label)] = {
    css.map(cs => pending(cs)).flatten
  }
  
/*  def matchNoPending(table: Table_, out: OutNeigh_, sorbe: Sorbe[ConstraintRef], open: Boolean): Boolean = {
    val css = filterCandidates(table, out, sorbe,open)
    !css.isEmpty && pendings(css).isEmpty
  } */
  
  private def matchCandidateSorbe(cs: Seq[Option[Candidate_]], 
      sorbe: Sorbe[ConstraintRef], 
      open: Boolean): Boolean = {
    println(s"--Checking candidate: $cs with sorbe: $sorbe. Bag: ${candidatesToBag(cs)}. Interval: ${sorbe.interval(candidatesToBag(cs))}")
    val b = !containsContradictions(cs) && 
            sorbe.contains(candidatesToBag(cs),open)
    println(s"--Result of checking candidate $cs with $sorbe = $b")
    b
  }
  
  // TODO: The following code could be optimized using some mathematical formula
  // A contradiction appears when a value N has sign -1 and another value N has sign +1
  private def containsContradictions(cs: Seq[Option[Candidate_]]): Boolean = {
    val defined = cs.filter(_.isDefined)
    val pos = defined.filter(_.get.sign == 1).map(_.get.value)
    val neg = defined.filter(_.get.sign == -1).map(_.get.value)
    pos.intersect(neg).length != 0
  }
  
  // TODO: It ignores extra predicates (value None) and negative candidates by now
  private def candidatesToBag(cs: Seq[Option[Candidate_]]): Bag[ConstraintRef] = {
    Bag.toBag(cs.filter(x => x.isDefined && x.get.sign == 1).map(_.get.value))
  }

  def zipCandidates(table: Table_, node: Node, out: Seq[(Edge,Node)]): Seq[Seq[Option[Candidate_]]] = {
    zipN(candidates(table,node, out))
  }
  
  private def calculateCandidates(
      table: Table_, 
      out: OutNeigh_, 
      sorbe: Sorbe[ConstraintRef],
      node: Node,
      open: Boolean): Try[Seq[Candidates_]] = {
    Try{
     filterCandidates(table,out,node,sorbe,open) 
    }
  }
  
  private def combineTypings(t1: Typing_, t2: Typing_): Try[Typing_] = {
    t1.combine(t2)
  }
  
  private def resolveCandidate(
      n: Node, 
      g: Graph_,
      s:Schema_)(
      c: Option[Candidate_],
      rest: Result_ 
      ): Result_ = {
    c match {
      case None => {
       rest 
      }
      case Some(Pending(c,obj,label,arc)) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => matchNodeInTyping(obj,label,g,result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult(arc,r)
        } 
      }
      
      case Some(Pos(ref,arc)) => { 
        // Basic matching with no pending
        // TODO: Accumulate triples checked?
        println(s"Basic candidate matched. Constraint: $ref, node: $n, arc: $arc" )
        addArcResult(arc,rest)
      }
      case Some(Neg(ref,arc)) => { 
        // TODO: Throw exception?
        println(s"Neg candidate: $ref. Node: $n. Arc = $arc")
        rest
      }

    }
  }
  
  private def addArcResult(arc: Arc_, result: Result_): Result_ = {
    result match {
      case Success(rs) => Success(rs.map((pair) => (pair._1,arc +: pair._2)))
      case f@Failure(_) => f
    }
  }
  private def resolveCandidates(
      cs: Candidates_, 
      node: Node, 
      label: Label,
      g: Graph_,
      s: Schema_,
      t: Typing_): Result_ = {
    // TODO: Check if we can return triples instead of Seq()
    val checked = Seq()
    val zero : Result_ = t.addPosType(node, label).map(t => Seq((t,checked))) 
    cs.foldRight(zero)(resolveCandidate(node, g, s))
  }
  
  // TODO: Move to utils 
  def filterSuccessSeq[A](ts: Seq[Try[Seq[A]]]): Try[Seq[Seq[A]]] = {
    filterSuccess(ts)
  }
  
  // TODO: Move to utils 
  def filterSuccessSeqFlatten[A](ts: Seq[Try[Seq[A]]]): Try[Seq[A]] = {
    filterSuccessSeq(ts).map(s => s.flatten)
  }
  
  private def resolveAllCandidates(
      css: Seq[Candidates_],
      node: Node, 
      label: Label,
      g: Graph_,
      s: Schema_,
      currentTyping: Typing_): Result_ = {
  val attempts : Seq[Result_] = 
    css.map(cs => resolveCandidates(cs,node,label,g,s,currentTyping))
  filterSuccessSeqFlatten(attempts)
  }
  
  /**
   * Matches a node with a label in a graph 
   * Takes into account the current result typing and triples
   */
  private def matchNodeInTyping(
      node: Node, 
      label: Label, 
      graph: Graph_,
      result: SingleResult_): Result_ = {
    println(s"Trying to match node $node with label $label, currentResult: $result" )

    val currentTyping = result._1
    // If the node has already been checked, return without checking again to avoid recursion
    // TODO: Maybe, we could try again in a more dynamic setting
    if (currentTyping.getLabels(node) contains label) {
      // val s : Seq[(Edge,Node)] = Seq()
      Success(Seq(result))
    }
    else {
    val out = graph.out(node)
    for {
      (table,sorbe) <- mkTable(label)
      val open = !m(label).closed
      allCandidates <- calculateCandidates(table,out,sorbe,node,open)
      newTyping <- currentTyping.addPosType(node,label)
      results <- resolveAllCandidates(allCandidates, node, label, graph, this, newTyping)
    } yield {
      if (m(label).closed) {
        // filter out results that don't affect all triples in out neighbourhood 
        results.filter(r => containsAllTriples(node,out,r._2))
      } else results
    }
  }
  }
  
  def containsAllTriples(node: Node, out: Seq[(Edge,Node)], triples: Seq[(Node,Edge,Node)]): Boolean = {
    val arcsWithNode = triples.filter(_._1 == node).map(triple => (triple._2,triple._3))
    out.forall(arcsWithNode contains _)
  }
  
  def matchNode(
      node: Node, 
      label: Label, 
      graph: Graph_): Result_ = {
    matchNodeInTyping(node,label,graph,(PosNegTyping.empty,Seq()))
  }
}

object Schema {
  def empty[Edge,Node,Label,Err] = Schema(Map())
}
