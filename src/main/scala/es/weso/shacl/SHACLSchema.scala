package es.weso.shacl
import scala.util.parsing.input.Positional

case class SHACLSchema(
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

object SHACLSchema {
  def empty: SHACLSchema = 
    SHACLSchema(
    id = None,
    valueClasses = Map(),
    shapes = Map(),
    start = None,
    startActions = Actions.empty)
}
