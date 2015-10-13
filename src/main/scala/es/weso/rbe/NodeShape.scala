package es.weso.rbe
import es.weso.utils.Checker

sealed trait NodeShape[+Label,+Node,+Err]

// Reference to another label
case class Ref[Label](label:Label) extends NodeShape[Label,Nothing,Nothing]

// Reference to another label
case class RefNot[Label](label:Label) extends NodeShape[Label,Nothing,Nothing]

// Reference to a sequence of labels
case class ConjRef[Label](labels:Seq[Label]) extends NodeShape[Label,Nothing,Nothing]

// Boolean Constraint on nodes (it has a name and a predicate)
// pred is defined in the 2nd parameter section to avoid equality and hashing of functions
case class Pred[Node,Err](name: String)
    (val pred:Node => Checker[Node,Err]) extends NodeShape[Nothing,Node,Err] {
  
  /* Overrides equals to be able to compare avoiding function comparison
  override def equals(other: Any) = {
    name == other.name
  } */
}

object NodeShape {
  
  def any[Node]: Pred[Node,Nothing] = 
      Pred("any")((node) => Checker.ok(node))
      
}
