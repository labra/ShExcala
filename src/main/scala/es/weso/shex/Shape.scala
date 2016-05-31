package es.weso.shex

import es.weso.rdf.nodes._
import scala.util.parsing.input.Positional

sealed abstract class Shape {
  def addActions(as:Actions): Shape
}


case class BasicShape(
    shapeExpr: ShapeExpr,
    isClosed: Boolean,
    isVirtual: Boolean,
    inherit: Seq[Label],
    extras: Seq[IRI],
    actions: Actions) extends Shape {
  
  override def addActions(as: Actions): Shape = {
    this.copy(actions = this.actions ++ as)
  }
  
}

object BasicShape {
  def empty : BasicShape = {
      BasicShape(
        shapeExpr = EmptyShape(),
        isClosed = false,
        isVirtual = false,
        inherit = List(),
        extras = List(),
        actions = Actions.empty)
  }
}
    
case class NodeKindShape(
    nodeKind: NodeKind,
    shape: Shape
    ) extends Shape {
  
  override def addActions(as: Actions): Shape = {
    this.copy(shape = shape.addActions(as))
  }
  
}
    
/*case class NotShape(shape: Shape
     ) extends Shape */

object Shape {
  lazy val empty: Shape = BasicShape.empty
  
  def fromShapeExpr(se: ShapeExpr): Shape = 
    BasicShape.empty.copy(shapeExpr = se) 
}

 
