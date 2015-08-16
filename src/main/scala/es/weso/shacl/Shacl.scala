package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shacl.PREFIXES._
import util._
import es.weso.utils.PrefixMapUtils._

/**
 * Shacl Abstract Syntax 
 */
object Shacl {

  /**
   * Represents a SHACL Schema
   */
  type Actions = Map[IRI,String]
  
  case class SHACLSchema(
    id: Option[Label],
    shapes: Map[Label,Shape],
    start: Option[Label],
    startActions: Actions)
      extends Positional // Positional helps Parser Combinators to show positions 
      {

    def findShape(label: Label): Option[Shape] = {
      shapes.get(label)
    }
    
    def labels: Set[Label] = {
      shapes.keySet
    }
  }
  
  object SHACLSchema {
    def empty : SHACLSchema = SHACLSchema(
        id = None,
        shapes = Map(),
        start = None,
        startActions = Map()
        ) 
  }
  
  type Shapes = Map[Label,Shape] 

  case class Shape(
    shapeExpr: ShapeExpr,
    isClosed: Boolean,
    isVirtual: Boolean,
    inherit: Set[Label],
    extras: Set[IRI],
    actions: Actions
  ) extends Positional
  
  object Shape {
    
    lazy val empty: Shape = 
       Shape(
           shapeExpr = EmptyShape, 
           isClosed=false,
           isVirtual=false,
           inherit = Set(),
           extras = Set(),
           actions = Map()
           )
  }

  sealed trait ShapeExpr extends Positional

/*  case class TripleConstraint(
    id: Option[Label],
    iri: IRI,
    value: ValueClass
    ) extends ShapeExpr */ 

  case class InverseTripleConstraint(
    id: Option[Label],
    iri: IRI,
    shape: ShapeConstr
    ) extends ShapeExpr

  case class TripleConstraintCard(
    id: Option[Label],
    iri: IRI,
    value: ValueClass,
    card: Cardinality
    ) extends ShapeExpr 

  case class InverseTripleConstraintCard(
    id: Option[Label],
    iri: IRI,
    shape: ShapeConstr,
    card: Cardinality
    ) extends ShapeExpr

  // Binary operators for convenience
    
  // Or(l,s1,s2) = SomeOfShape(l,List(s1,s2))
  case class Or(id: Option[Label], shape1: ShapeExpr, shape2: ShapeExpr) extends ShapeExpr
  
  // Or(l,s1,s2) = SomeOfShape(l,List(s1,s2))
  case class XOr(id: Option[Label], shape1: ShapeExpr, shape2: ShapeExpr) extends ShapeExpr
  
  // Group(l,s1,s2) = GroupShape(l,s1,s2)
  case class Group2(id: Option[Label],shape1: ShapeExpr, shape2: ShapeExpr) extends ShapeExpr 
  
  // XOr(l,s1,s2) = OneOfShape(l,List(s1,s2))
  case class OneOf(id: Option[Label], shapes: List[ShapeExpr]) extends ShapeExpr
  
  // List-based operators (semantically, they can be defined in terms of the binary operators
  
  // SomeOfShape(id,List(s1,s2,s3)) = Or(id,s1,Or(_,s2,Or(_,s2,EmptyShape)))
  case class SomeOf(id: Option[Label],shapes: Seq[ShapeExpr]) extends ShapeExpr
  
  // GroupShape(id,List(s1,s2,s3)) = Group2(id,s1,Group2(_,s2,Group2(_,s2,EmptyShape)))
  case class GroupShape(
      id: Option[Label], 
      shapes: Seq[ShapeExpr]) extends ShapeExpr

  case class RepetitionShape(
      id:Option[Label],
      shape: ShapeExpr, 
      card:Cardinality) extends ShapeExpr  {
    def minusOne: RepetitionShape = {
      this.copy(card = card.minusOne)
    }
  }
  
  case object EmptyShape extends ShapeExpr

  /**
   * Labels
   */
  sealed trait Label {
    def getNode(): RDFNode
    
    def show(implicit pm:PrefixMap): String
  }

  case class IRILabel(iri: IRI) extends Label {
    override def getNode = iri
    
    override def show(implicit pm:PrefixMap): String = {
      qualify(iri)
    }
  }

  case class BNodeLabel(bnode: BNodeId) extends Label {
    override def getNode = bnode
    
    override def show(implicit pm:PrefixMap): String = 
      "_:" + bnode.id 
  }

  def mkLabel(str: String): IRILabel = {
    IRILabel(IRI(str))
  }
  
  def mkLabel(node: RDFNode): Label = {
    node match {
      case b: BNodeId => BNodeLabel(b)
      case i: IRI => IRILabel(i)
      case _ => throw new Exception("Cannot convert node " + node + " to Label")
    }
  }

  /**
   * ValueClass ::= ValueConstr | ShapeConstr
   * 
   */
  sealed trait ValueClass extends Positional

  /**
   * ValueConstr ::= LiteralDatatype | ValueSet | NodeKind 
   */
  sealed trait ValueConstr extends ValueClass
    with Positional
    
  case class LiteralDatatype(
    v: RDFNode,
    facets: Seq[XSFacet]) extends ValueConstr
    with Positional

  case class ValueSet(s: Seq[ValueObject]) extends ValueConstr
    with Positional

  sealed trait ValueObject extends Positional
  
  case class ValueIRI(iri: IRI) extends ValueObject
  case class ValueLiteral(literal: Literal) extends ValueObject
  case class ValueLang(lang: Lang) extends ValueObject
  case class ValueStem(stem: IRI, exclusions: List[Exclusion]) extends ValueObject
  case class ValueAny(exclusions: List[Exclusion]) extends ValueObject
  
  case class Exclusion(iri: IRI, isStem: Boolean) extends Positional

  sealed trait NodeKind extends ValueConstr 
     with Positional {
    def token: String
  }

  case class IRIKind(facets: List[StringFacet]) extends NodeKind {
    override def token = "IRI"
  }

  case class BNodeKind(facets: List[StringFacet]) extends NodeKind {
    override def token = "BNode"
  }

  case class LiteralKind(facets: List[XSFacet]) extends NodeKind {
    override def token = "Literal"
  }

  case class NonLiteralKind(facets: List[XSFacet]) extends NodeKind {
    override def token = "NonLiteral"
  }

  // This kind is not in the spec...but it may useful
  case object AnyKind extends NodeKind {
    override def token = "Any"
  }

  def nodeKindfromIRI(iri: IRI): Try[NodeKind] = {
    iri match {
      case `sh_IRI` => Success(IRIKind(List()))
      case `sh_BNode` => Success(BNodeKind(List()))
      case `sh_Literal` => Success(LiteralKind(List()))
      case `sh_NonLiteral` => Success(NonLiteralKind(List()))
      case `sh_Any` => Success(AnyKind)
      case _ => Failure(new Exception("nodeKindFromIRI: unsupported IRI: " + iri))
    }
  }

  sealed trait XSFacet extends Positional
  
  sealed trait NumericFacet extends XSFacet with Positional
  case class MinInclusive(n: Integer) extends NumericFacet
  case class MinExclusive(n: Integer) extends NumericFacet
  case class MaxInclusive(n: Integer) extends NumericFacet
  case class MaxExclusive(n: Integer) extends NumericFacet
  case class TotalDigits(n: Integer) extends NumericFacet
  case class FractionDigits(n: Integer) extends NumericFacet
  
  sealed trait StringFacet extends XSFacet with Positional
  
  case class Pattern(regex: Regex) extends StringFacet
  case class Length(n: Integer) extends StringFacet
  case class MinLength(n: Integer) extends StringFacet
  case class MaxLength(n: Integer) extends StringFacet

  /**
   * ShapeConstr ::= SingleShape | DisjShapeConstr | ConjShapeConstr | NotShapeConstr
   */
  sealed trait ShapeConstr extends ValueClass
    with Positional

  case class SingleShape(shape: Label) extends ShapeConstr 
  case class DisjShapeConstr(shapes: Set[Label]) extends ShapeConstr
  case class ConjShapeConstr(shapes: Set[Label]) extends ShapeConstr
  case class NotShapeConstr(shape: ShapeConstr) extends ShapeConstr


  case class Action(
      label: Label,
      action: String)

  sealed trait Cardinality extends Positional {
    def minusOne : Cardinality 
  }

  case class RangeCardinality(m: Int, n: Int) extends Cardinality {
    require(m >= 0)
    require(m <= n) 
    
    def minusOne = 
      this match {
      case RangeCardinality(0,0) => this
      case RangeCardinality(0,n) if n > 0 => RangeCardinality(0, n - 1)
      case RangeCardinality(m,n) if m > 0 && n > 0 => RangeCardinality(m - 1, n - 1)
      case _ => throw new Exception("minusOne: Unexpected cardinality " + this)
    }
  }

  /**
   * UnboundedCardinality represents ranges (m,unbounded)
   */
  case class UnboundedCardinalityFrom(m: Int) extends Cardinality {
    require(m >= 0)

    def minusOne = 
      this match {
      case UnboundedCardinalityFrom(0) => UnboundedCardinalityFrom(0)
      case UnboundedCardinalityFrom(n) if n > 0 => UnboundedCardinalityFrom(m - 1) 
      case _ => throw new Exception("minusOne: Unexpected cardinality " + this)
    }
  }

  lazy val NoActions: Seq[Action] = Seq()

  // lazy val NoId : Label = IRILabel(iri = IRI(""))
   lazy val iriKind = IRIKind(List())
   lazy val bnodeKind = BNodeKind(List())

  
  lazy val typeXsdString = LiteralDatatype(xsd_string, List())
  lazy val star = UnboundedCardinalityFrom(0)
  lazy val plus = UnboundedCardinalityFrom(1)
  lazy val optional = RangeCardinality(0, 1)
  
  lazy val defaultCardinality = RangeCardinality(1,1)
  lazy val emptyFacets : Seq[XSFacet] = Seq() 
  def defaultMaxCardinality(m:Int) = RangeCardinality(1,m)
    
  lazy val emptyInclPropSet: Set[IRI] = Set()
}