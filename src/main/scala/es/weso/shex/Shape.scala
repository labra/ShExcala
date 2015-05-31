package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shacl.PREFIXES._

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 */
object ShapeSyntax {

  case class ShEx(rules: Seq[Shape], start: Option[Label])
      extends Positional // Positional helps Parser Combinators to show positions
      with SchemaImpl {

    def findShape(label: Label): Option[Shape] = {
      val rs = rules.filter(r => r.label == label)
      if (rs.size == 1) Some(rs.head)
      else None
    }
  }

  case class Shape(label: Label, rule: Rule)
    extends Positional

  sealed trait Rule extends Positional

  case class ArcRule(id: Option[Label], n: NameClass, v: ValueClass) extends Rule
  case class RevArcRule(id: Option[Label], n: NameClass, v: ValueClass) extends Rule
  case class RelationRule(id: Option[Label], v1: ValueClass, v2: ValueClass) extends Rule
  case class AndRule(r1: Rule, r2: Rule) extends Rule
  case class OrRule(r1: Rule, r2: Rule) extends Rule
  case class StarRule(r: Rule) extends Rule
  case class PlusRule(r: Rule) extends Rule
  case class OptRule(r: Rule) extends Rule
  case class NotRule(r: Rule) extends Rule
  case class ActionRule(r: Rule, a: Seq[Action]) extends Rule
  case class RangeRule(m: Int, n: Int, r: Rule) extends Rule
  case class RangeMinRule(m: Int, r: Rule) extends Rule
  case object EmptyRule extends Rule
  case object AnyRule extends Rule
  case class FailRule(msg: String) extends Rule // Always fails with a message
  case class OpenRule(r: Rule) extends Rule

  sealed trait Label {
    def getNode(): RDFNode
  }

  case class IRILabel(iri: IRI) extends Label {
    override def getNode = iri
  }

  case class BNodeLabel(bnode: BNodeId) extends Label {
    override def getNode = bnode
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

  sealed trait NameClass
  case class NameTerm(t: IRI) extends NameClass
  case class NameAny(excl: Set[IRIStem]) extends NameClass
  case class NameStem(s: IRIStem) extends NameClass

  sealed trait ValueClass
  case class ValueType(v: RDFNode) extends ValueClass
  case class ValueSet(s: Seq[ValueObject]) extends ValueClass
  case class ValueAny(excl: Set[IRIStem]) extends ValueClass
  case class ValueStem(s: IRIStem) extends ValueClass
  case class ValueReference(l: Label) extends ValueClass

  sealed trait ValueObject

  case class RDFNodeObject(node: RDFNode) extends ValueObject
  case class LangObject(lang: Lang) extends ValueObject
  case class RegexObject(regex: Regex, lang: Option[Lang]) extends ValueObject
  case class NoObject(obj: ValueObject) extends ValueObject
  case class OrObject(obj1: ValueObject, obj2: ValueObject) extends ValueObject

  case class Action(label: Label, code: String)

  /*
 Can be defined as: 
 option(r) = OrRule(r,EmptyRule)
 */
  def option(r: Rule): Rule = {
    OptRule(r)
  }

  /* Can be defined as:
 * star(r) = OrRule(PlusRule(r),EmptyRule)
 */
  def star(r: Rule): Rule = {
    StarRule(r)
  }

  /* Can be defined as:
 def rangeMin(m:Int)(r:Rule): Rule = {
  require(m >= 0, "range: m must not be negative")
  if (m == 0) {
    EmptyRule
  } else {
    AndRule(r,rangeMin(m-1)(r))
  }   
} */
  def rangeMin(m: Int)(r: Rule) = RangeMinRule(m, r)

  /* Can be defined as: 
def range(m:Int,n:Int)(r:Rule): Rule = {
  require(m >= 0, "range: m must not be negative")
  require(n >= m,"range: n(" + n + ") must be bigger than m (" + m + ")")
  if (m == 0) {
    if (n == 0) EmptyRule
    else AndRule(OptRule(r),range(0,n - 1)(r))
  } else {
    AndRule(r,range(m-1,n-1)(r))
  }   
} */
  def range(m: Int, n: Int)(r: Rule) = RangeRule(m, n, r)

  def openShape(r: Rule): Rule = {
    AndRule(r, AnyRule)
  }

  lazy val NoActions: Seq[Action] = Seq()

  // lazy val NoId : Label = IRILabel(iri = IRI(""))

  case class IRIStem(iri: IRI, isStem: Boolean) {

    def matchStem(other: RDFNode): Boolean = {
      other.isIRI &&
        other.toIRI.str.startsWith(iri.str)
    }

  }

  lazy val typeShLiteral = ValueType(v = sh_Literal)
  lazy val typeShIRI = ValueType(v = sh_IRI)
  lazy val typeShBNode = ValueType(v = sh_BNode)

  lazy val typeShexNonLiteral = ValueType(v = sh_NonLiteral)
  lazy val typeShexNonIRI = ValueType(v = sh_NonIRI)
  lazy val typeShexNonBNode = ValueType(v = sh_NonBNode)

  lazy val typeXsdString = ValueType(v = xsd_string)

  lazy val anyShape = Shape(label = IRILabel(sh_Any), rule = AnyRule
  )

  def matchStems(stems: Set[IRIStem], node: RDFNode): Boolean = {
    stems.exists(_.matchStem(node))
  }

}