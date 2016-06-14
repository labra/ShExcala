package es.weso.shex
import scala.util.parsing.input.Positional
import es.weso.utils.Debugging
import es.weso.rdf.nodes._
import NumericFacetTypeClass._
import PREFIXES._
import es.weso.validating._
import Checked._
import Constraint._
import scala.util.matching.Regex
import cats._
import cats.implicits._

sealed trait XSFacet extends Positional with Debugging {
  def check(node: RDFNode): CheckedRDFNode
}

object XSFacet extends Debugging {


  def numericDatatype(dt: IRI): Boolean = {
    dt match {
      case `xsd_integer` => true
      case `xsd_double`  => true
      case `xsd_decimal` => true
      case _             => false
    }
  }


  def checkFacets(
      node: RDFNode,
      facets: Seq[XSFacet]): CheckedRDFNode = {
    if (facets.isEmpty) okSingle(node, s"$node matches empty facets")
    else {
      val fns : Seq[RDFNode => CheckedRDFNode] = facets.map(f => f.check _)
      val r : CheckedRDFNode = checkValueAll(node, fns)
      r
    }
  }

  def ok_facets(iri: IRI, facets: List[XSFacet]): Boolean = {
    facets.forall { case f => ok_facet(f, iri) }
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
        debug(s"ok_facet: Unsupported facet type: $facet")
        false
      }
    }
  }

  lazy val emptyFacets: List[XSFacet] = List()

}
sealed trait NumericFacet extends XSFacet with Positional {

  def cnvChecked[A]
  ( c: Checked[A,ConstraintReason,ConstraintError[A]],
    f: A => RDFNode): CheckedRDFNode = {
    c.mapValue(f(_)).mapErrors(Functor[ConstraintError].lift(f(_)))
  }

}

case class MinInclusive(n: Integer) extends NumericFacet {
  import NumericFacetTypeClass._
  lazy val name = "MinInclusive"


  def check(node: RDFNode): CheckedRDFNode = {
    node match {
      case IntegerLiteral(x) => {
        val facet = NumericFacetInteger
        cnvChecked(facet.minInclusive(x, n),facet.toRDFNode)
      }
      case DecimalLiteral(x) => {
        val facet = NumericFacetDecimal
        cnvChecked(facet.minInclusive(x, n.toDouble),facet.toRDFNode)
      }
      case DoubleLiteral(x)  => {
        val facet = NumericFacetDouble
        cnvChecked(facet.minInclusive(x, n.toDouble),facet.toRDFNode)
      }
      case l: Literal if XSFacet.numericDatatype(l.dataType) => {
        val facet = NumericFacetString
        cnvChecked(facet.minInclusive(l.getLexicalForm, n.toString),facet.toRDFNode)
      }
      case _ => errString(s"MinInclusive($n) failed with node $node")
    }
  }
}
 case class MinExclusive(n: Integer) extends NumericFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    node match {
      case IntegerLiteral(x) => {
        val facet = NumericFacetInteger
        cnvChecked(facet.minExclusive(x, n), facet.toRDFNode)
      }
      case DecimalLiteral(x) => {
        val facet = NumericFacetDecimal
        cnvChecked(facet.minExclusive(x, n.toDouble), facet.toRDFNode)
      }
      case DoubleLiteral(x) => {
        val facet = NumericFacetDouble
        cnvChecked(facet.minExclusive(x, n.toDouble), facet.toRDFNode)
      }
      case l: Literal if XSFacet.numericDatatype(l.dataType) => {
        val facet = NumericFacetString
        cnvChecked(facet.minExclusive(l.getLexicalForm, n.toString), facet.toRDFNode)
      }
      case _ => errString(s"MinExclusive($n) failed with node $node")
    }
  }
}

case class MaxInclusive(n: Integer) extends NumericFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    node match {
      case IntegerLiteral(x) => {
        val facet = NumericFacetInteger
        cnvChecked(facet.maxInclusive(x, n),facet.toRDFNode)
      }
      case DecimalLiteral(x) => {
        val facet = NumericFacetDecimal
        cnvChecked(facet.maxInclusive(x, n.toDouble),facet.toRDFNode)
      }
      case DoubleLiteral(x)  => {
        val facet = NumericFacetDouble
        cnvChecked(facet.maxInclusive(x, n.toDouble),facet.toRDFNode)
      }
      case l: Literal if XSFacet.numericDatatype(l.dataType) => {
        val facet = NumericFacetString
        cnvChecked(facet.maxInclusive(l.getLexicalForm, n.toString),facet.toRDFNode)
      }
      case _ => errString(s"MaxInclusive($n) failed with node $node")
    }
  }
}
case class MaxExclusive(n: Integer) extends NumericFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    node match {
      case IntegerLiteral(x) => {
        val facet = NumericFacetInteger
        cnvChecked(facet.maxExclusive(x, n),facet.toRDFNode)
      }
      case DecimalLiteral(x) => {
        val facet = NumericFacetDecimal
        cnvChecked(facet.maxExclusive(x, n.toDouble),facet.toRDFNode)
      }
      case DoubleLiteral(x)  => {
        val facet = NumericFacetDouble
        cnvChecked(facet.maxExclusive(x, n.toDouble),facet.toRDFNode)
      }
      case l: Literal if XSFacet.numericDatatype(l.dataType) => {
        val facet = NumericFacetString
        cnvChecked(facet.maxExclusive(l.getLexicalForm, n.toString),facet.toRDFNode)
      }
      case _ => errString(s"MaxExclusive($n) failed with node $node")
    }
  }
}

case class TotalDigits(n: Integer) extends NumericFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    node match {
      case IntegerLiteral(x) => {
        val facet = NumericFacetInteger
        cnvChecked(facet.checkTotalDigits(x, n),facet.toRDFNode)
      }
      case DecimalLiteral(x) => {
        val facet = NumericFacetDecimal
        cnvChecked(facet.checkTotalDigits(x, n),facet.toRDFNode)
      }
      case DoubleLiteral(x)  => {
        val facet = NumericFacetDouble
        cnvChecked(facet.checkTotalDigits(x, n),facet.toRDFNode)
      }
      case l: Literal if XSFacet.numericDatatype(l.dataType) => {
        val facet = NumericFacetString
        cnvChecked(facet.checkTotalDigits(l.getLexicalForm, n),facet.toRDFNode)
      }
      case _ => errString(s"TotalDigits($n) failed with node $node")
    }
  }
}
case class FractionDigits(n: Integer) extends NumericFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    node match {
      case IntegerLiteral(x) => {
        val facet = NumericFacetInteger
        cnvChecked(facet.checkFractionDigits(x, n),facet.toRDFNode)
      }
      case DecimalLiteral(x) => {
        val facet = NumericFacetDecimal
        cnvChecked(facet.checkFractionDigits(x, n),facet.toRDFNode)
      }
      case DoubleLiteral(x)  => {
        val facet = NumericFacetDouble
        cnvChecked(facet.checkFractionDigits(x, n),facet.toRDFNode)
      }
      case l: Literal if XSFacet.numericDatatype(l.dataType) => {
        val facet = NumericFacetString
        cnvChecked(facet.checkFractionDigits(l.getLexicalForm, n),facet.toRDFNode)
      }
      case _ => errString(s"FractionDigits($n) failed with node $node")
    }
  }
}

sealed trait StringFacet extends XSFacet with Positional

case class Pattern(regexStr: String) extends StringFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    debug(s"Checking pattern $regexStr facet on node $node")
    val regex : Regex = regexStr.r
    debug(s"Regex = \\$regex\\")
    val str = node.getLexicalForm
    regex.findFirstIn(str) match {
      case Some(_) => okSingle(node, s"$node matches regular expression $regexStr")
      case None    => errString(s"Facet pattern($regex) doesn't match node $node with lexical form $str")
    }
  }
}

case class Length(n: Integer) extends StringFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    val str = node.getLexicalForm
    if (str.length == n) okSingle(node,s"$node matches length facet $n")
    else errString(s"Facet length($n) doesn't match node $node with lexical form $str")
  }
}
case class MinLength(n: Integer) extends StringFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    val str = node.getLexicalForm
    if (str.length >= n) okSingle(node, s"$node matches MinLength $n")
    else errString(s"Facet MinLength($n) doesn't match node $node with lexical form $str")
  }
}
case class MaxLength(n: Integer) extends StringFacet {
  def check(node: RDFNode): CheckedRDFNode = {
    val str = node.getLexicalForm
    if (str.length <= n) okSingle(node,s"$node matches MaxLength $n")
    else errString(s"Facet MaxLength($n) doesn't match node $node with lexical form $str")
  }
}


