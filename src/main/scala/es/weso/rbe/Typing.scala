package es.weso.rbe

import scalaz._
import Scalaz._
import util.{Try,Success,Failure}

trait ReasonPosNegTyping[Node,Label, ReasonPos, ReasonNeg] {
  def getPosTypesReason(node: Node): Seq[(Label,ReasonPos)]
  def getNegTypesReason(node: Node): Seq[(Label,ReasonNeg)]
  def getAllTypesReason(node: Node): (Seq[(Label,ReasonPos)],Seq[(Label,ReasonNeg)])
  def addPosTypeReason(node: Node, label: Label, reasonPos: ReasonPos): Try[ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg]]
  def addNegTypeReason(node: Node, label: Label, reasonNeg: ReasonNeg): Try[ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg]]
  def nodes: Seq[Node]
} 

object ReasonPosNegTyping {
  def empty[Node,Label,ReasonPos,ReasonNeg]: ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg] = {
    ReasonTypingAsMap(Map())
  }
}


trait PosTyping[Node,Label] {
  def getPosTypes(node: Node): Seq[(Label)]
  def addPosType(node: Node, label: Label): Try[PosNegTyping[Node,Label]]
  def nodes: Seq[Node]
} 

trait PosNegTyping[Node,Label] extends PosTyping[Node,Label] {
  def getNegTypes(node: Node): Seq[(Label)]
  def getLabels(node: Node): Seq[Label]
  def getAllTypes(node: Node): (Seq[(Label)],Seq[(Label)])
  def addNegType(node: Node, label: Label): Try[PosNegTyping[Node,Label]]
  def combine(other: PosNegTyping[Node,Label]): Try[PosNegTyping[Node,Label]]
  def addTypeRow(node: Node, tr: TypeRow[Label]): Try[PosNegTyping[Node,Label]]
  def asMap: Map[Node,TypeRow[Label]]
} 
object PosNegTyping {
  def empty[Node,Label] = PosNegTypingAsMap[Node,Label](Map())
  
  def fromPosMap[Node,Label](m: Map[Node,Set[Label]]) =
    PosNegTypingAsMap(m.mapValues(labels => TypeRow(labels, Set())))
    
  def fromPosNegMap[Node,Label](m: Map[Node,(Set[Label],Set[Label])]) =
    PosNegTypingAsMap(m.mapValues(pair => TypeRow(pair._1, pair._2)))
}

case class TypeFail(msg:String) extends Exception(msg)

case class TypeRow[Label](pos: Set[Label], neg: Set[Label]) {
  
  def combine(other: TypeRow[Label]): Try[TypeRow[Label]] = {
    for {
      t1 <- this.addPosSet(other.pos)
      t2 <- t1.addNegSet(other.neg)
    } yield t2
  }

  // TODO: Refactor the following code to DRY
  
  def addPosSet(s: Set[Label]): Try[TypeRow[Label]] = {
    val zero : Try[TypeRow[Label]] = Success(this)
    s.foldLeft(zero) {
      case (Success(rest),label) => rest.addPos(label)
      case (Failure(e),_) => Failure(e)
    }
  }
  
  def addNegSet(s: Set[Label]): Try[TypeRow[Label]] = {
    val zero : Try[TypeRow[Label]] = Success(this)
    s.foldLeft(zero) {
      case (Success(rest),label) => rest.addNeg(label)
      case (Failure(e),_) => Failure(e)
    }

  }
  
  def addPos(label: Label): Try[TypeRow[Label]] = {
    if (negLabels contains label) 
      Failure(TypeFail(s"addPosType: label $label is already in negLabels. Current typeRow: $this"))
    else 
      if (posLabels contains label) {
        Success(this)  // TODO: Should we combine reasonPos?
      } else {
        Success(this.copy(pos = pos + (label)))
      }
  }

    def addNeg(label: Label): Try[TypeRow[Label]] = { 
    if (posLabels contains label) 
      Failure(TypeFail(s"addNegType: label $label is already in posLabels. Current typeRow: $this"))
    else 
      if (negLabels contains label) {
        Success(this)  
      } else {
        Success(this.copy(neg = neg + (label)))
      }
  }
  
  lazy val negLabels : Set[Label] = {
    neg
  }
  
  lazy val posLabels : Set[Label] = {
    pos
  }
  
  override def toString = {
    val sb = new StringBuilder
    if (pos.isEmpty && neg.isEmpty) sb ++= "()"
    else {
      if (!pos.isEmpty) {
       if (pos.size == 1) {
        sb ++= "+" + pos.head.toString
       }
       else {
        sb ++= "+(" + pos.map(_.toString).mkString(",") + ")"        
       }
     }
     if (!neg.isEmpty) { 
      if (neg.size == 1) {
        sb ++= "-" + neg.head.toString
      }
      else {
        sb ++= "-(" + neg.map(_.toString).mkString(",") + ")"        
      }
     }
    }
    sb.toString
  }
}

object TypeRow {
  def empty[Label] = 
    TypeRow(
        pos = Set[Label](), 
        neg = Set[Label]())
        
}


case class ReasonTypeRow[Label,ReasonPos,ReasonNeg](
    pos: Set[(Label,ReasonPos)],
    neg: Set[(Label,ReasonNeg)]) {
  
  def addPos(label : Label, reasonPos: ReasonPos): Try[ReasonTypeRow[Label,ReasonPos,ReasonNeg]] = { 
    if (negLabels contains label) 
      Failure(TypeFail(s"addPos: label $label is already in negLabels. Current typeRow: $this"))
    else 
      if (posLabels contains label) {
        Success(this)  // TODO: Should we combine reasonPos?
      } else {
        Success(this.copy(pos = pos + ((label,reasonPos))))
      }
  }
  
  def addNeg(label: Label,reasonNeg: ReasonNeg): Try[ReasonTypeRow[Label,ReasonPos,ReasonNeg]] = { 
    if (posLabels contains label) 
      Failure(TypeFail(s"addNegType: label $label is already in posLabels. Current typeRow: $this"))
    else 
      if (negLabels contains label) {
        Success(this)  // TODO: Should we combine reasonPos?
      } else {
        Success(this.copy(neg = neg + ((label,reasonNeg))))
      }
  }
  
  lazy val negLabels : Set[Label] = {
    neg.map(_._1)
  }
  
  lazy val posLabels : Set[Label] = {
    pos.map(_._1)
  }
  
  override def toString = {
    val sb = new StringBuilder
    if (pos.isEmpty && neg.isEmpty) sb ++= "()"
    else {
      if (!pos.isEmpty) {
       if (pos.size == 1) {
        sb ++= "+" + pos.head.toString
       }
       else {
        sb ++= "+(" + pos.map(_.toString).mkString(",") + ")"        
       }
     }
     if (!neg.isEmpty) { 
      if (neg.size == 1) {
        sb ++= "-" + neg.head.toString
      }
      else {
        sb ++= "-(" + neg.map(_.toString).mkString(",") + ")"        
      }
     }
    }
    sb.toString
  }
    
}

object ReasonTypeRow {
  def empty[Label,ReasonPos,ReasonNeg] = 
    ReasonTypeRow(
        pos = Set[(Label,ReasonPos)](), 
        neg = Set[(Label,ReasonNeg)]())
        
}

case class ReasonTypingAsMap[Node, Label, ReasonPos, ReasonNeg](
    m: Map[Node,ReasonTypeRow[Label,ReasonPos,ReasonNeg]]
    ) extends ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg] {
  
  override def nodes = m.keys.toSeq

  override def addPosTypeReason(n: Node, label: Label, reason: ReasonPos) = {
    val typeRow = 
      if (m contains n) m(n)
      else ReasonTypeRow.empty[Label,ReasonPos,ReasonNeg]
    for {
        tr <- typeRow.addPos(label,reason)
    } yield {
        ReasonTypingAsMap(m + (n -> tr))
    }
  }
  
  override def addNegTypeReason(n: Node, label: Label, reason: ReasonNeg) = {
    val typeRow = 
      if (m contains n) m(n)
      else ReasonTypeRow.empty[Label,ReasonPos,ReasonNeg]
    
    for {
        tr <- typeRow.addNeg(label,reason)
    } yield {
        ReasonTypingAsMap(m + (n -> tr))
    }
  }

  override def getPosTypesReason(node: Node): Seq[(Label,ReasonPos)] = {
    m.get(node).getOrElse(ReasonTypeRow.empty).pos.toSeq
  }
  
  override def getNegTypesReason(node: Node): Seq[(Label,ReasonNeg)] = {
    m.get(node).getOrElse(ReasonTypeRow.empty).neg.toSeq
  }
  override def getAllTypesReason(node: Node): (Seq[(Label,ReasonPos)],Seq[(Label,ReasonNeg)]) = {
    (getPosTypesReason(node),getNegTypesReason(node))
  }
  
  override def toString: String = {
    "Typing:" + m.toString
  }

}

case class PosNegTypingAsMap[Node,Label](m: Map[Node,TypeRow[Label]]) 
  extends PosNegTyping[Node,Label] {
  
  override def nodes = m.keys.toSeq
  
  override def addPosType(node: Node,label: Label): Try[PosNegTypingAsMap[Node,Label]] = {
    val typeRow =  
      if (m contains node) m(node)
      else TypeRow.empty[Label]
    for {
        tr <- typeRow.addPos(label)
    } yield {
        PosNegTypingAsMap(m + (node -> tr))
    }
  }
  
  override def addNegType(node: Node,label: Label): Try[PosNegTypingAsMap[Node,Label]] = {
    val typeRow =  
      if (m contains node) m(node)
      else TypeRow.empty[Label]
    for {
        tr <- typeRow.addNeg(label)
    } yield {
        PosNegTypingAsMap(m + (node -> tr))
    }
  }
  
  override def getPosTypes(node: Node): Seq[(Label)] = m.get(node).getOrElse(TypeRow.empty).pos.toSeq
  override def getNegTypes(node: Node): Seq[(Label)] = m.get(node).getOrElse(TypeRow.empty).neg.toSeq
  override def getAllTypes(node: Node): (Seq[(Label)],Seq[(Label)]) = (getPosTypes(node),getNegTypes(node))
  override def getLabels(node: Node): Seq[Label] = getPosTypes(node) ++ getNegTypes(node)
  override def asMap = m
  override def combine(other: PosNegTyping[Node,Label]): Try[PosNegTyping[Node,Label]] = {
    val zero : Try[PosNegTyping[Node,Label]] = Success(this)
    other.asMap.foldLeft(zero){
      case (Success(rest),(node,typeRow)) => rest.addTypeRow(node, typeRow)
      case (Failure(e),_) => Failure(e)
    }
  }
  
  def addTypeRow(node: Node, tr: TypeRow[Label]): Try[PosNegTyping[Node,Label]] = {
    //TODO: Simplify the following expression
    if (m contains node) {
      for {
        tr1 <- m(node).combine(tr)
      } yield PosNegTypingAsMap(m = m.updated(node,tr1))
    } else {
     Success(PosNegTypingAsMap(m + (node -> tr))) 
    }
  } 
}

object PosNegTypingAsMap {
  def empty[Node,Label]: PosNegTyping[Node,Label] = {
    PosNegTypingAsMap(Map())
  }
  
}

