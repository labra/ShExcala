package es.weso.shex

import es.weso.rdf.nodes._
import scala.util.parsing.input.Positional


case class Shape(
    shapeExpr: ShapeExpr,
    isClosed: Boolean,
    isVirtual: Boolean,
    inherit: Seq[Label],
    extras: Seq[IRI],
    actions: Actions) extends Positional

object Shape {
    lazy val empty: Shape =
      Shape(
        shapeExpr = EmptyShape(),
        isClosed = false,
        isVirtual = false,
        inherit = List(),
        extras = List(),
        actions = Actions.empty)
  }
