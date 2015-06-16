package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._

import es.weso.rdf._
import scala.util._
import es.weso.shacl.Shacl._
import es.weso.utils.PrefixMapUtils._

case class TypingException(msg:String) extends Exception {
  new Exception(msg)
}

case class Reason(msg:String)

case class ShapeType(
    shapes: Set[Label], 
    negShapes: Set[(Label,Reason)]
    ) {
  
  lazy val negShapesLabels : Set[Label] = negShapes.map(_._1)
  
  def addShape(shape: Label): Try[ShapeType] = {
    if (negShapesLabels contains shape) 
      Failure(TypingException("addShape: shape " + shape + " belongs to negShapes " + negShapes))
    else 
      Success(this.copy(shapes = shapes + shape))
  }
  
  def addNegShape(label: Label, reason: Reason): Try[ShapeType] = {
    if (shapes contains label) 
      Failure(TypingException("addNegShape: shape " + label + " belongs to shapes " + shapes))
    else 
      Success(this.copy(negShapes = negShapes + ((label,reason)) 
          ))
  }
  
  def combine(other: ShapeType): Try[ShapeType] = {
    for {
     check1 <- check("combine shapeType: intersection between " + other.negShapes.toString + " and " + shapes.toString + " + must be empty",
                     other.negShapesLabels.intersect(shapes) == Set())
     check2 <- check("combine shapeType: intersection between " + other.shapes.toString + " and " + negShapes.toString + " + must be empty",
                     other.shapes.intersect(negShapesLabels) == Set())
                     
    } yield 
       ShapeType(shapes = shapes ++ other.shapes, negShapes = negShapes ++ other.negShapes)
  }
  

  def check(msg: String, cond:Boolean): Try[Boolean] = {
    if (cond) Success(true)
    else Failure(TypingException(msg))
  }
  
  def show(implicit pm: PrefixMap): String = {
    val sb = new StringBuilder
    if (shapes.isEmpty && negShapes.isEmpty) {
      sb ++= "()"
    }
    if (!shapes.isEmpty) {
      if (shapes.size == 1) {
        sb ++= "+" + shapes.map(_.show).mkString
      }
      else {
        sb ++= "+(" + shapes.map(_.show).mkString(",") + ")"        
      }
    }  
    if (!negShapes.isEmpty) {
      if (negShapes.size == 1) sb ++= "-" + shapes.map(_.show).mkString 
      else {
        sb ++= "+(" + shapes.map(_.show).mkString(",") + ")"        
      }
    }  
   sb.toString  
  }
}

object ShapeType {
  
  def apply() : ShapeType = 
    ShapeType(shapes =Set(), negShapes= Set())
  
  def shape(shape: Label): ShapeType =
    ShapeType(shapes=Set(shape), negShapes=Set())
  
  def negShape(shape: Label, reason: Reason): ShapeType =
    ShapeType(shapes=Set(), negShapes=Set((shape, reason)))
    
  def combineShapeTypes(s1: ShapeType, s2:ShapeType): Try[ShapeType] = 
    s1.combine(s1)

}

case class Typing(map: Map[RDFNode, ShapeType]) {

  def addShape(
      key: RDFNode, 
      shapeLabel: Label): Try[Typing] = {
    if (map contains key) {
      for {
        newShapeType <- map(key).addShape(shapeLabel)
      } yield Typing(map.updated(key,newShapeType))
    } else {
      Success(Typing(map + ((key, ShapeType.shape(shapeLabel)))))
    }
  }

  def addNegShape(
      key: RDFNode, 
      shapeLabel: Label,
      reason: Reason): Try[Typing] = {
    if (map contains key) {
      for {
        newShapeType <- map(key).addNegShape(shapeLabel,reason)
      } yield Typing(map.updated(key,newShapeType))
    } else {
      Success(Typing(map + ((key, ShapeType.negShape(shapeLabel,reason)))))
    }
  }
  
  
/*  def rmType(key: RDFNode, value: RDFNode): Option[Typing] = {
    if ((map contains key) && (map(key) contains value)) {
      val newSet = map(key) - value
      if (newSet.isEmpty) {
        Some(Typing(map - key))
      } else {
        Some(Typing(map.updated(key, newSet)))
      }
    } else None
  } */

  def hasShapes(key: RDFNode): Set[Label] = {
    if (map contains key) map(key).shapes
    else Set()
  }

  def hasNegShapesLabels(key: RDFNode): Set[Label] = {
    if (map contains key) map(key).negShapesLabels
    else Set()
  }
  
  
  def combineMaps[K,V](map1: Map[K,V], map2: Map[K,V], comb: (V,V) => Try[V]): Try[Map[K,V]] = {

    def combineMap(rest: Try[Map[K,V]], current:(K,V)): Try[Map[K,V]] = {
     val (k,v) = current
     rest match {
       case Failure(_) => rest
       case Success(map) => {
         if (map contains k)
           for {
             newV <- comb(map(k),v) 
           } yield map.updated(k,newV)
         else 
           Success(map.updated(k,v))
       }
     }
    }
    val empty : Try[Map[K,V]] = Success(Map())
    
    map1.foldLeft(empty)(combineMap)
  }
  
  def combine(other: Typing): Try[Typing] = {
    for {
     newMap <- combineMaps(map,other.map,ShapeType.combineShapeTypes) 
    } yield Typing(newMap)
  }

  def containsType(n: RDFNode, label: Label): Boolean = {
    hasShapes(n) contains label
  } 

  def showTyping(implicit pm: PrefixMap): String = {
    val sb = new StringBuilder
    for (is <- map) {
      sb ++= (showNode(is._1) + " -> " + is._2.show + "\n")
    }
    sb.toString
  }

  // TODO: Refactor to put these 2 definitions in RDF
  private def showIRI(iri: IRI)(implicit pm: PrefixMap): String = {
    "<" + iri.str + ">"
  }

  private def showNode(node: RDFNode)(implicit pm: PrefixMap): String = {
    if (node.isIRI) qualify(node.toIRI)
    else node.toString
  }

}

object Typing {

  def emptyTyping: Typing = 
    Typing(Map[RDFNode, ShapeType]())

}