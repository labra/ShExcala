package es.weso.shex

case class ValueClassDefinition(
    defn: Either[(ValueClass,Actions), External]
)

object ValueClassDefinition {
  
  def fromValueClass(vc: ValueClass): ValueClassDefinition = {
    ValueClassDefinition(Left((vc,Actions.empty)))
  }
  
  def fromValueClassActions(vc: ValueClass, as: Actions): ValueClassDefinition = {
    ValueClassDefinition(Left((vc,as)))
  }
  
  def external: ValueClassDefinition = {
    ValueClassDefinition(Right(External()))
  }
  
}

case class External()
