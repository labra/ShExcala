package es.weso.shex
import scala.util.parsing.input.Positional

case class ShExSchema(
  id: Option[Label],
  valueClasses: Map[Label,ValueClassDefinition],
  shapes: Map[Label, Shape],
  start: Option[Label],
  startActions: Actions)
    extends Positional // Positional helps Parser Combinators to show positions 
    {

  def findShape(label: Label): Option[Shape] = {
    shapes.get(label)
  }

  def labels: List[Label] = {
    shapes.keys.toList
  }
}

object ShExSchema {
  def empty: ShExSchema = 
    ShExSchema(
    id = None,
    valueClasses = Map(),
    shapes = Map(),
    start = None,
    startActions = Actions.empty)
}
