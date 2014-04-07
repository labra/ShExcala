package es.weso.shex

import es.weso.rdfNode.IRI
import es.weso.parser.PrefixMap
import es.weso.rdfNode.RDFNode
import scala.util.parsing.input.Positional

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 * */

object ShapeSyntax {
  
case class ShEx(rules:Seq[Shape], start: Option[Label])

case class Shape(label: Label, rule: Rule)


sealed trait Rule 
	   extends Positional  // Positional help Parser Combinators to show positions

case class ArcRule(
    id: Option[Label],
    n: NameClass,
    v: ValueClass,
    c: Cardinality,
    a: Seq[Action]
    ) extends Rule
    
case class AndRule(e1: Rule, e2: Rule) extends Rule
case class OrRule(e1: Rule, e2: Rule) extends Rule
case object NoRule extends Rule

// Using recursive syntax trees we don't need GroupRule 
//case class GroupRule(rule: Rule, opt: Boolean, a: Seq[Action]) extends Rule

sealed trait Label
case class IRILabel(iri: IRI) extends Label
case class BNodeLabel(bnodeId: Int) extends Label

case class IRIStem(iri: IRI, isStem: Boolean)

sealed trait NameClass
case class NameTerm(t: IRI) extends NameClass
case class NameAny(excl: Set[IRIStem]) extends NameClass
case class NameStem(s: IRI) extends NameClass

sealed trait ValueClass
case class ValueType(v: RDFNode) extends ValueClass
case class ValueSet(s: Seq[RDFNode]) extends ValueClass
case class ValueAny(stem: IRIStem) extends ValueClass
case class ValueStem(s: IRI) extends ValueClass
case class ValueReference(l: Label) extends ValueClass

case class Action(label: Label, code: String)

// Utility definitions 

case class Cardinality(min: Integer,max: Either[Integer,Unbound])
case class Unbound()

lazy val unbound : Unbound = Unbound()
lazy val Default = Cardinality(min = 1, max=Left(1))
lazy val Plus = Cardinality(min = 1, max=Right(unbound))
lazy val Star = Cardinality(min = 0, max=Right(unbound))
lazy val Opt  = Cardinality(min = 0, max=Left(1))

lazy val NoActions : Seq[Action] = Seq()
// lazy val NoId : Label = IRILabel(iri = IRI(""))

def range(m: Integer, n: Integer): Cardinality = {
  require(n > m)
  Cardinality(min = m, max = Left(n))
}

lazy val foaf = "http://xmlns.com/foaf/0.1/"
lazy val xsd  = "http://www.w3.org/2001/XMLSchema#"
lazy val shex = "http://www.w3.org/2013/ShEx/ns#"
lazy val typeShexLiteral  	= ValueType(v = IRI(shex + "Literal"))
lazy val typeShexIRI  		= ValueType(v = IRI(shex + "IRI"))
lazy val typeShexBNode  	= ValueType(v = IRI(shex + "BNode"))
lazy val typeShexNonLiteral	= ValueType(v = IRI(shex + "NonLiteral"))
lazy val typeXsdString		= ValueType(v = IRI(xsd  + "string"))


}