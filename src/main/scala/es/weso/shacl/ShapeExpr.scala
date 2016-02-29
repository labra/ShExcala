package es.weso.shacl
import scala.util.parsing.input.Positional
import es.weso.rdf.nodes._
import ValueClass.any
import Cardinality.defaultCardinality
import PREFIXES.rdf_type

  sealed trait ShapeExpr extends Positional {
    def addId(label: Label): ShapeExpr
  }

  case class TripleConstraint(
      id: Option[Label],
      iri: IRI,
      value: ValueClass,
      card: Cardinality,
      inverse: Boolean,
      negated: Boolean,
      annotations: List[Annotation],
      actions: Actions) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  object TripleConstraint {
    def empty = TripleConstraint(
      id = None,
      iri = rdf_type,
      value = any,
      card = defaultCardinality,
      inverse = false,
      negated = false,
      annotations = List(),
      actions = Actions.empty)
  }


  // Binary operators for convenience

  // Or(l,s1,s2) = SomeOfShape(l,List(s1,s2))
  case class Or(
      id: Option[Label],
      shape1: ShapeExpr,
      shape2: ShapeExpr) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  // XOr(l,s1,s2) = OneOfShape(l,List(s1,s2))
  /*case class XOr(
      id: Option[Label], 
      shape1: ShapeExpr, 
      shape2: ShapeExpr) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }*/

  // Group2(l,s1,s2) = Group(l,s1,s2)
  case class Group2(
      id: Option[Label],
      shape1: ShapeExpr,
      shape2: ShapeExpr) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  // OneOf(l,s1,s2) = OneOf(l,List(s1,s2))
  /*case class OneOf(
      id: Option[Label], 
      shapes: List[ShapeExpr]) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  } */

  // List-based operators (semantically, they can be defined in terms of the binary operators

  // SomeOfShape(id,List(s1,s2,s3)) = Or(id,s1,Or(_,s2,Or(_,s2,EmptyShape)))
  case class SomeOf(
      id: Option[Label],
      shapes: Seq[ShapeExpr]) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  // GroupShape(id,List(s1,s2,s3)) = Group2(id,s1,Group2(_,s2,Group2(_,s2,EmptyShape)))
  case class GroupShape(
      id: Option[Label],
      shapes: Seq[ShapeExpr]) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  case class RepetitionShape(
      id: Option[Label],
      shape: ShapeExpr,
      card: Cardinality,
      annotations: List[Annotation],
      actions: Actions) extends ShapeExpr {
    def minusOne: RepetitionShape = {
      this.copy(card = card.minusOne)
    }

    def addId(label: Label) =
      this.copy(id = Some(label))

  }

  case class IncludeShape(
      id: Option[Label],
      label: Label) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  case class EmptyShape(
      id: Option[Label]) extends ShapeExpr {
    def addId(label: Label) = this.copy(id = Some(label))
  }

  object EmptyShape {
    def apply(): EmptyShape = EmptyShape(id = None)
  }
