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
  case class SHACLSchema(
    id: Option[Label],
    shapes: Map[Label,Shape],
    start: Option[Label],
    startActions: Map[IRI,String])
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
    inherit: Set[IRI]
  ) extends Positional
  
  object Shape {
    
    lazy val empty: Shape = 
       Shape(shapeExpr = EmptyShape, 
           isClosed=false, 
           Set())
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

  // Or(l,s1,s2) = SomeOfShape(l,List(s1,s2))
  case class Or(id: Option[Label], shape1: ShapeExpr, shape2: ShapeExpr) extends ShapeExpr
  
  // XOr(l,s1,s2) = OneOfShape(l,List(s1,s2))
  case class XOr(id: Option[Label], shape1: ShapeExpr, shape2: ShapeExpr) extends ShapeExpr
  
  // Group(l,s1,s2) = GroupShape(l,s1,s2)
  case class Group2(id: Option[Label],shape1: ShapeExpr, shape2: ShapeExpr) extends ShapeExpr 
  
  // SomeOfShape(id,List(s1,s2,s3)) = Or(id,s1,Or(_,s2,Or(_,s2,EmptyShape)))
  case class SomeOfShape(id: Option[Label],shapes: Seq[ShapeExpr]) extends ShapeExpr
  
  // SomeOfShape(id,List(s1,s2,s3)) = XOr(id,s1,XOr(_,s2,XOr(_,s2,EmptyShape)))
  case class OneOfShape(id:Option[Label], shapes: Seq[ShapeExpr]) extends ShapeExpr
  
  // GroupShape(id,List(s1,s2,s3)) = Group2(id,s1,Group2(_,s2,Group2(_,s2,EmptyShape)))
  case class GroupShape(id: Option[Label], shapes: Seq[ShapeExpr]) extends ShapeExpr

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

  sealed trait NodeKind extends ValueConstr 
     with Positional {
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
  
  case object AnyKind extends NodeKind {
    override def token = "Any"
  }

  def nodeKindfromIRI(iri: IRI): Try[NodeKind] = {
    iri match {
      case `sh_IRI` => Success(IRIKind)
      case `sh_BNode` => Success(BNodeKind)
      case `sh_Literal` => Success(LiteralKind)
      case `sh_NonLiteral` => Success(NonLiteralKind)
      case `sh_Any` => Success(AnyKind)
      case _ => Failure(new Exception("nodeKindFromIRI: unsupported IRI: " + iri))
    }
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

  lazy val typeXsdString = LiteralDatatype(xsd_string, List())
  lazy val star = UnboundedCardinalityFrom(0)
  lazy val plus = UnboundedCardinalityFrom(1)
  lazy val optional = RangeCardinality(0, 1)
  
  lazy val defaultCardinality = RangeCardinality(1,1)
  lazy val emptyFacets : Seq[XSFacet] = Seq() 
  def defaultMaxCardinality(m:Int) = RangeCardinality(1,m)
    
  lazy val emptyInclPropSet: Set[IRI] = Set()
}