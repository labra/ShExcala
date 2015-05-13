package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shex.PREFIXES._

/**
 * The following definitions follow: http://w3c.github.io/data-shapes/semantics/
 */
object Shacl {

  /**
   * Represents a SHACL Schema
   */
  case class SHACLSchema(
    id: Option[Label],
    rules: Seq[Rule],
    start: Option[Label])
      extends Positional // Positional helps Parser Combinators to show positions 
      {

    def findShape(label: Label): Option[Rule] = {
      val rs = rules.filter(_.label == label)
      if (rs.size == 1) Some(rs.head)
      else None
    }
  }

  case class Rule(
    label: Label,
    shapeDefinition: ShapeDefinition,
    extensionCondition: Seq[ExtensionCondition]) extends Positional

  sealed trait ShapeDefinition extends Positional

  case class OpenShape(
    id: Option[Label],
    shape: ShapeExpr,
    inclPropSet: Set[IRI]) extends ShapeDefinition

  case class ClosedShape(
    id: Option[Label],
    shape: ShapeExpr) extends ShapeDefinition

  sealed trait ShapeExpr extends Positional

  case class TripleConstraint(
    id: Option[Label],
    iri: IRI,
    value: ValueClass,
    card: Cardinality)
      extends ShapeExpr

  case class InverseTripleConstraint(
    id: Option[Label],
    iri: IRI,
    shape: ShapeConstr,
    card: Cardinality) extends ShapeExpr

  trait ValueClass extends Positional

  case class SomeOfShape(shapes: Seq[ShapeExpr]) extends ShapeExpr
  case class OneOfShape(shapes: Seq[ShapeExpr]) extends ShapeExpr
  case class GroupShape(shapes: Seq[ShapeExpr]) extends ShapeExpr
  case object EmptyShape extends ShapeExpr

  sealed trait Label {
    def getNode(): RDFNode
  }

  case class IRILabel(iri: IRI) extends Label {
    override def getNode = iri
  }

  case class BNodeLabel(bnodeId: Int) extends Label {
    override def getNode = BNodeId(bnodeId)
  }

  def mkLabel(str: String): IRILabel = {
    IRILabel(IRI(str))
  }

  sealed trait ValueConstr
    extends ValueClass
    with Positional
  case class LiteralDatatype(
    v: RDFNode,
    facets: Seq[XSFacet]) extends ValueConstr

  case class ValueSet(s: Seq[ValueObject]) extends ValueConstr

  sealed trait NodeKind extends ValueConstr {
    def token: String
  }

  case object IRIKind extends NodeKind {
    override def token = "IRI"
  }

  case object BNodeKind extends NodeKind {
    override def token = "BNode"
  }

  case object LiteralKind extends NodeKind {
    override def token = "Literal"
  }

  case object NonLiteralKind extends NodeKind {
    override def token = "NonLiteral"
  }

  sealed trait XSFacet extends Positional
  case class Pattern(regex: Regex) extends XSFacet
  case class MinInclusive(n: Integer) extends XSFacet
  case class MinExclusive(n: Integer) extends XSFacet
  case class MaxInclusive(n: Integer) extends XSFacet
  case class MaxExclusive(n: Integer) extends XSFacet
  case class Length(n: Integer) extends XSFacet
  case class MinLength(n: Integer) extends XSFacet
  case class MaxLength(n: Integer) extends XSFacet
  case class TotalDigits(n: Integer) extends XSFacet
  case class FractionDigits(n: Integer) extends XSFacet

  sealed trait ShapeConstr
    extends ValueClass
    with Positional

  case class DisjShapeConstr(shapes: Set[Label]) extends ShapeConstr
  case class ConjShapeConstr(shapes: Set[Label]) extends ShapeConstr
  case class NotShapeConstr(shape: ShapeConstr) extends ShapeConstr

  sealed trait ValueObject extends Positional
  case class ValueIRI(iri: IRI) extends ValueObject
  case class ValueLiteral(literal: Literal) extends ValueObject
  case class ValueLang(lang: Lang) extends ValueObject

  /*
  case class RDFNodeObject(node: RDFNode) extends ValueObject
  case class LangObject(lang: Lang) extends ValueObject
  case class RegexObject(regex: Regex, lang: Option[Lang]) extends ValueObject
  case class NoObject(obj: ValueObject) extends ValueObject
  case class OrObject(obj1: ValueObject, obj2: ValueObject) extends ValueObject */

  case class ExtensionCondition(
    extLangName: Label,
    extDefinition: String)

  sealed trait Cardinality extends Positional

  case class RangeCardinality(m: Int, n: Int) extends Cardinality {
    require(m >= 0)
    require(m < n)
  }

  /**
   * UnboundedCardinality represents ranges (m,unbounded)
   */
  case class UnboundedCardinalityFrom(m: Int) extends Cardinality {
    require(m >= 0)
  }

  lazy val NoActions: Seq[ExtensionCondition] = Seq()

  // lazy val NoId : Label = IRILabel(iri = IRI(""))

  lazy val typeXsdString = LiteralDatatype(xsd_string, List())
  lazy val noExtension: Seq[ExtensionCondition] = List()
  lazy val star = UnboundedCardinalityFrom(0)
  lazy val plus = UnboundedCardinalityFrom(1)
  lazy val optional = RangeCardinality(0, 1)
  lazy val defaultCardinality = UnboundedCardinalityFrom(1)
  lazy val emptyInclPropSet: Set[IRI] = Set()
}