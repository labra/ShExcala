package es.weso.rbe
import es.weso.utils.Checker

/**
 * A node shape
 */
sealed trait NodeShape[+Label,+Node,+Err] {
}

/**
 *  Reference to another label
 */
case class Ref[Label](label:Label) extends NodeShape[Label,Nothing,Nothing] {
}

/**
 * Negation of a expression
 */
case class RefNot[Label](label:Label) extends NodeShape[Label,Nothing,Nothing]

/**
 *  Reference to a sequence of labels
 */
case class ConjRef[Label](labels:Seq[Label]) extends NodeShape[Label,Nothing,Nothing]

/**
 *  Reference to an sequence of labels which are disjunctive
 */
case class DisjRef[Label](labels:Seq[Label]) extends NodeShape[Label,Nothing,Nothing]

case class OrShape[+Label,+Node,+Err](ns:Seq[NodeShape[Label,Node,Err]]) extends NodeShape[Label,Node,Err]

/**
 *  Boolean Constraint on nodes (it has a name and a predicate).
 *  
 *  Note: pred is defined in the 2nd parameter section to avoid equality and hashing of functions
 *  
 */
case class Pred[Node,Err](name: String)
    (val pred:Node => Checker[Node,Err]) extends NodeShape[Nothing,Node,Err] {
  
}

/**
 * Some common node shapes
 */
object NodeShape {
  
  /**
   * any = any value matches, so no constraint at all
   */
  def any[Node]: Pred[Node,Nothing] = 
      Pred("any")((node) => Checker.ok(node))
      
}
