package es.weso.shacl
import scala.util.parsing.input.Positional
import es.weso.utils._
import es.weso.rdf.nodes._
import Checker._
import NumericFacetTypeClass._
import PREFIXES._
import org.slf4j._
import org.apache.log4j.LogManager
import scala.util.matching.Regex

sealed trait XSFacet extends Positional with Logging {
  def check(node: RDFNode): Checker[RDFNode, ValidationError]
}

object XSFacet {
  
  lazy val log = LogManager.getLogger("Logging")

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
      facets: List[XSFacet]): Checker[RDFNode, ValidationError] = {
    if (facets.isEmpty) ok(node)
    else {
      Checker.checkValueAll(node, facets.map(_.check _))
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
        log.info(s"ok_facet: Unsupported facet type: $facet")
        false
      }
    }
  }

  lazy val emptyFacets: List[XSFacet] = List()

}
sealed trait NumericFacet extends XSFacet with Positional {
}

case class MinInclusive(n: Integer) extends NumericFacet {
  import NumericFacetTypeClass._
  lazy val name = "MinInclusive"

  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val cmp = node match {
      case IntegerLiteral(x) => NumericFacetInteger.minInclusive(x, n)
      case DecimalLiteral(x) => NumericFacetDecimal.minInclusive(x, n.toDouble)
      case DoubleLiteral(x)  => NumericFacetDouble.minInclusive(x, n.toDouble)
      case l: Literal if XSFacet.numericDatatype(l.dataType) =>
        NumericFacetString.minInclusive(l.getLexicalForm, n.toString)
      case _ => err(MsgError(s"MinInclusive($n) failed with node $node"))
    }
    cmp.map(_ => node)
  }
}
case class MinExclusive(n: Integer) extends NumericFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val cmp = node match {
      case IntegerLiteral(x) => NumericFacetInteger.minExclusive(x, n)
      case DecimalLiteral(x) => NumericFacetDecimal.minExclusive(x, n.toDouble)
      case DoubleLiteral(x)  => NumericFacetDouble.minExclusive(x, n.toDouble)
      case l: Literal if XSFacet.numericDatatype(l.dataType) =>
        NumericFacetString.minExclusive(l.getLexicalForm, n.toString)
      case _ => err(MsgError(s"MinExclusive($n) failed with node $node"))
    }
    cmp.map(_ => node)
  }

}
case class MaxInclusive(n: Integer) extends NumericFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val cmp = node match {
      case IntegerLiteral(x) => NumericFacetInteger.maxInclusive(x, n)
      case DecimalLiteral(x) => NumericFacetDecimal.maxInclusive(x, n.toDouble)
      case DoubleLiteral(x)  => NumericFacetDouble.maxInclusive(x, n.toDouble)
      case l: Literal if XSFacet.numericDatatype(l.dataType) =>
        NumericFacetString.maxInclusive(l.getLexicalForm, n.toString)
      case _ => err(MsgError(s"MaxInclusive($n) failed with node $node"))
    }
    cmp.map(_ => node)
  }
}
case class MaxExclusive(n: Integer) extends NumericFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val cmp = node match {
      case IntegerLiteral(x) => NumericFacetInteger.maxExclusive(x, n)
      case DecimalLiteral(x) => NumericFacetDecimal.maxExclusive(x, n.toDouble)
      case DoubleLiteral(x)  => NumericFacetDouble.maxExclusive(x, n.toDouble)
      case l: Literal if XSFacet.numericDatatype(l.dataType) =>
        NumericFacetString.maxExclusive(l.getLexicalForm, n.toString)
      case _ => err(MsgError(s"MaxExclusive($n) failed with node $node"))
    }
    cmp.map(_ => node)
  }
}
case class TotalDigits(n: Integer) extends NumericFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val cmp = node match {
      case IntegerLiteral(x) => NumericFacetInteger.checkTotalDigits(x, n)
      case DecimalLiteral(x) => NumericFacetDecimal.checkTotalDigits(x, n)
      case DoubleLiteral(x)  => NumericFacetDouble.checkTotalDigits(x, n)
      case l: Literal if XSFacet.numericDatatype(l.dataType) =>
        NumericFacetString.checkTotalDigits(l.getLexicalForm, n)
      case _ => err(MsgError(s"CheckTotalDigits($n) failed with node $node"))
    }
    cmp.map(_ => node)
  }
}
case class FractionDigits(n: Integer) extends NumericFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val cmp = node match {
      case IntegerLiteral(x) => NumericFacetInteger.checkFractionDigits(x, n)
      case DecimalLiteral(x) => NumericFacetDecimal.checkFractionDigits(x, n)
      case DoubleLiteral(x)  => NumericFacetDouble.checkFractionDigits(x, n)
      case l: Literal if XSFacet.numericDatatype(l.dataType) =>
        NumericFacetString.checkFractionDigits(l.getLexicalForm, n)
      case _ => err(MsgError(s"checkFractionDigits($n) failed with node $node"))
    }
    cmp.map(_ => node)
  }
}

sealed trait StringFacet extends XSFacet with Positional

case class Pattern(regexStr: String) extends StringFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    log.info(s"Checking pattern $regexStr facet on node $node")
    val regex : Regex = regexStr.r
    log.info(s"Regex = \\$regex\\")
    val str = node.getLexicalForm
    regex.findFirstIn(str) match {
      case Some(_) => ok(node)
      case None    => err(MsgError(s"Facet pattern($regex) doesn't match node $node with lexical form $str"))
    }
  }
}

case class Length(n: Integer) extends StringFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val str = node.getLexicalForm
    if (str.length == n) ok(node)
    else err(MsgError(s"Facet length($n) doesn't match node $node with lexical form $str"))
  }
}
case class MinLength(n: Integer) extends StringFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val str = node.getLexicalForm
    if (str.length >= n) ok(node)
    else err(MsgError(s"Facet MinLength($n) doesn't match node $node with lexical form $str"))
  }
}
case class MaxLength(n: Integer) extends StringFacet {
  def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    val str = node.getLexicalForm
    if (str.length <= n) ok(node)
    else err(MsgError(s"Facet MaxLength($n) doesn't match node $node with lexical form $str"))
  }
}


