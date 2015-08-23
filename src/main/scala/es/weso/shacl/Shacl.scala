package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shacl.PREFIXES._
import util._
import es.weso.utils.PrefixMapUtils._
import org.slf4j._


/**
 * Shacl Abstract Syntax 
 */
object Shacl {

  val log = LoggerFactory.getLogger("Shacl")

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
    
    def labels: List[Label] = {
      shapes.keys.toList
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
    inherit: List[Label],
    extras: List[IRI],
    actions: Actions
  ) extends Positional
  
  object Shape {
    
    lazy val empty: Shape = 
       Shape(
           shapeExpr = EmptyShape(), 
           isClosed=false,
           isVirtual=false,
           inherit = List(),
           extras = List(),
           actions = Map()
           )
  }

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
    actions: Actions
    ) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
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
        actions = Map()
        )
  }

  case class Annotation(
      iri: IRI, 
      value: Either[IRI,Literal]
  )

  // Binary operators for convenience

  
  // Or(l,s1,s2) = SomeOfShape(l,List(s1,s2))
  case class Or(
      id: Option[Label], 
      shape1: ShapeExpr, 
      shape2: ShapeExpr) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }
  
  // XOr(l,s1,s2) = OneOfShape(l,List(s1,s2))
  case class XOr(
      id: Option[Label], 
      shape1: ShapeExpr, 
      shape2: ShapeExpr) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }
  
  // Group2(l,s1,s2) = Group(l,s1,s2)
  case class Group2(
      id: Option[Label],
      shape1: ShapeExpr, 
      shape2: ShapeExpr) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  } 
  
  // OneOf(l,s1,s2) = OneOf(l,List(s1,s2))
  case class OneOf(
      id: Option[Label], 
      shapes: List[ShapeExpr]) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }
  
  // List-based operators (semantically, they can be defined in terms of the binary operators
  
  // SomeOfShape(id,List(s1,s2,s3)) = Or(id,s1,Or(_,s2,Or(_,s2,EmptyShape)))
  case class SomeOf(
      id: Option[Label],
      shapes: Seq[ShapeExpr]) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }
  
  // GroupShape(id,List(s1,s2,s3)) = Group2(id,s1,Group2(_,s2,Group2(_,s2,EmptyShape)))
  case class GroupShape(
      id: Option[Label], 
      shapes: Seq[ShapeExpr]) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }

  case class RepetitionShape(
      id:Option[Label],
      shape: ShapeExpr, 
      card:Cardinality,
      actions: Actions) extends ShapeExpr  {
    def minusOne: RepetitionShape = {
      this.copy(card = card.minusOne)
    }
    
    def addId(label:Label) = 
      this.copy(id = Some(label))
  
  }

  case class IncludeShape(
      id: Option[Label],
      label: Label
  ) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }
  
  case class EmptyShape (
      id: Option[Label]
      ) extends ShapeExpr {
    def addId(label:Label) = this.copy(id = Some(label))
  }
  
  object EmptyShape {
    def apply(): EmptyShape = EmptyShape(id = None)
  }

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
    
  case class Datatype(
    v: IRI,
    facets: List[XSFacet]) extends ValueConstr
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
  
  lazy val any = ValueSet(Seq(ValueAny(exclusions = List())))

  case class IRIKind(
      shapeConstr: Option[ShapeConstr], 
      facets: List[StringFacet]) extends NodeKind {
    override def token = "IRI"
  }

  case class BNodeKind(
      shapeConstr: Option[ShapeConstr], 
      facets: List[StringFacet]) extends NodeKind {
    override def token = "BNode"
  }

  case class LiteralKind(
      facets: List[XSFacet]) extends NodeKind {
    override def token = "Literal"
  }

  case class NonLiteralKind(
      shapeConstr: Option[ShapeConstr],
      facets: List[XSFacet]) extends NodeKind {
    override def token = "NonLiteral"
  }


  def nodeKindfromIRI(iri: IRI): Try[NodeKind] = {
    iri match {
      case `sh_IRI` => Success(IRIKind(None,List()))
      case `sh_BNode` => Success(BNodeKind(None,List()))
      case `sh_Literal` => Success(LiteralKind(List()))
      case `sh_NonLiteral` => Success(NonLiteralKind(None,List()))
//      case `sh_Any` => Success(AnyKind)
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
  
  case class Pattern(regex: String) extends StringFacet
  case class Length(n: Integer) extends StringFacet
  case class MinLength(n: Integer) extends StringFacet
  case class MaxLength(n: Integer) extends StringFacet
  
  
  // Does this function already exist?
  def all[A](ls: List[A], check: A =>Boolean): Boolean = {
    ls.foldLeft(true){case (rest,a) => check(a) && rest}
  }
  
  def ok_facets(iri:IRI, facets: List[XSFacet]): Boolean = {
    facets.forall{ case f => ok_facet(f, iri) }
  }

  def ok_facet(facet: XSFacet, iri: IRI): Boolean = {
    //TODO: verify the conditions for ok facets
    // At this moment, it allows numeric facets to 
    // builtin datatypes and any facet to other datatypes
    facet match {
      case _: NumericFacet => 
        builtinTypes.contains(iri)
      case _: StringFacet =>
        true // builtinTypes.contains(iri)
      case _ => {
        log.info(s"ok_facet: Unsupported facet type: $facet")
        false
      }
    }
  }
  /**
   * ShapeConstr ::= SingleShape | DisjShapeConstr | ConjShapeConstr | NotShapeConstr
   */
  sealed trait ShapeConstr extends ValueClass
    with Positional

  case class SingleShape(shape: Label) extends ShapeConstr 
  case class DisjShapeConstr(shapes: List[Label]) extends ShapeConstr
//  case class ConjShapeConstr(shapes: Set[Label]) extends ShapeConstr
//  case class NotShapeConstr(shape: ShapeConstr) extends ShapeConstr


  case class Action(
      label: Label,
      action: String)

  sealed trait Cardinality extends Positional {
    def minusOne : Cardinality
    def getMin: Int
    def getMax: Option[Int]
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
    
    def getMin = m
    
    def getMax = Some(n)
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
    
    def getMin = m
    
    def getMax = None
  }

  lazy val NoActions: Seq[Action] = Seq()

  // lazy val NoId : Label = IRILabel(iri = IRI(""))
  lazy val iriKind = IRIKind(None,List())
  lazy val bnodeKind = BNodeKind(None,List())

  
  lazy val typeXsdString = Datatype(xsd_string, List())
  lazy val builtinTypes = List(xsd_integer, xsd_double, xsd_string)
  
  lazy val star = UnboundedCardinalityFrom(0)
  lazy val plus = UnboundedCardinalityFrom(1)
  lazy val optional = RangeCardinality(0, 1)
  
  lazy val defaultCardinality = RangeCardinality(1,1)
  lazy val emptyFacets : List[XSFacet] = List() 
  def defaultMaxCardinality(m:Int) = RangeCardinality(1,m)
    
  lazy val emptyInclPropSet: List[IRI] = List()
}
