package es.weso.shacl

import scala.util.parsing.input.Positional
import es.weso.rdfgraph.nodes._
import es.weso.utils.Checker._
import es.weso.utils.{ Success => _, _}
import XSFacet._
import PREFIXES._
import util._

/**
 * ValueClass ::= ValueConstr | ShapeConstr | ValueClassRef
 *
 */
sealed trait ValueClass extends Positional

object ValueClass {
  lazy val any : ValueClass = ValueSet(Seq(ValueAny(exclusions = List())))
  
    def nodeKindfromIRI(iri: IRI): Try[NodeKind] = {
    iri match {
      case `sh_IRI`        => Success(IRIKind(None, List()))
      case `sh_BNode`      => Success(BNodeKind(None, List()))
      case `sh_Literal`    => Success(LiteralKind(List()))
      case `sh_NonLiteral` => Success(NonLiteralKind(None, List()))
      //      case `sh_Any` => Success(AnyKind)
      case _               => Failure(new Exception("nodeKindFromIRI: unsupported IRI: " + iri))
    }
  }

  lazy val iriKind = IRIKind(None, List())
  lazy val bnodeKind = BNodeKind(None, List())

  lazy val typeXsdString = Datatype(xsd_string, List())

}

case class ValueClassRef(label: Label) extends ValueClass


/**
 * ValueConstr ::= LiteralDatatype | ValueSet | NodeKind
 */
sealed trait ValueConstr extends ValueClass
    with Positional {
  def check(node: RDFNode): Checker[RDFNode, ValidationError]
}


case class Datatype(
  v: IRI,
  facets: List[XSFacet]) extends ValueConstr
    with Positional {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    if (v == sh_text) {
      node match {
      case l: Literal =>
        if ((l.dataType == xsd_string || l.dataType == rdf_langString) && checkFacets(node, facets).isOK)
          ok(node)
        else
          err(MsgError(s"literal $l doesn't match datatype $this"))
      case _ => err(MsgError(s"node $node doesn't match datatype $this"))
      }
    } else 
    node match {
      case l: Literal =>
        if (l.dataType == v && checkFacets(node, facets).isOK)
          ok(node)
        else
          err(MsgError(s"literal $l with datatype ${l.dataType} doesn't match datatype $v"))
      case _ => err(MsgError(s"node $node doesn't match datatype $this"))
    }
  }
}

case class ValueSet(s: Seq[ValueObject]) extends ValueConstr
    with Positional {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] =
    checkSome(node,
      s.map((vo: ValueObject) => ((n: RDFNode) => vo.check(node))),
      MsgError(s"valueSet: node $node must be in $s"))
}

sealed trait ValueObject extends Positional {
  def check(node: RDFNode): Checker[RDFNode, ValidationError]
}

case class ValueIRI(iri: IRI) extends ValueObject {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    if (node.isIRI && node.toIRI == iri) ok(node)
    else err(MsgError(s"node $node doesn't match IRI $iri"))
  }
}

case class ValueLiteral(literal: Literal) extends ValueObject {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    node match {
      case l: Literal if (l == literal) => ok(node)
      case _                            => err(MsgError(s"node $node doesn't match Literal $literal"))
    }
  }

}

case class ValueLang(lang: Lang) extends ValueObject {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    node match {
      case l: LangLiteral if (l.lang == lang) => ok(node)
      case _                                  => err(MsgError(s"node $node doesn't match Language literal $lang"))
    }
  }
}

case class ValueStem(stem: IRI, exclusions: List[Exclusion]) extends ValueObject {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    err(MsgError(s"Unimplemented value Stem"))
  }
}

case class ValueAny(exclusions: List[Exclusion]) extends ValueObject {
  override def check(node: RDFNode): Checker[RDFNode, ValidationError] = {
    if (exclusions.isEmpty) ok(node)
    else err(MsgError("Not implemented ValueAny with exclusions"))
  }
}

case class Exclusion(iri: IRI, isStem: Boolean) extends Positional

sealed trait NodeKind extends ValueConstr
    with Positional {
  def token: String

  override def check(node: RDFNode): Checker[RDFNode, ValidationError] =
    err(MsgError(s"Not implemented check on $this for node $node"))

}

case class IRIKind(
    shapeConstr: Option[ShapeConstr],
    facets: List[StringFacet]) extends NodeKind {
  override def token = "IRI"

  override def check(node: RDFNode): Checker[RDFNode, ValidationError] =
    if (shapeConstr.isDefined)
      err(MsgError(s"IRIKind: $this, unimplemented check for shapeConstr. Node: $node"))
    else {
      if (node.isIRI)
        checkFacets(node, facets)
      else {
        err(MsgError(s"IRIKind failed: node: $node is not an IRI"))
      }
    }
}

case class BNodeKind(
    shapeConstr: Option[ShapeConstr],
    facets: List[StringFacet]) extends NodeKind {
  override def token = "BNode"

  override def check(node: RDFNode): Checker[RDFNode, ValidationError] =
    if (shapeConstr.isDefined)
      err(MsgError(s"BNodeKind: $this, unimplemented check for shapeConstr. Node: $node"))
    else {
      if (node.isBNode)
        checkFacets(node, facets)
      else {
        // TODO...pass facets to error message
        err(MsgError(s"BNodeKind failed: node: $node is not an BNode or doesn't pass facets"))
      }
    }

}

case class LiteralKind(
    facets: List[XSFacet]) extends NodeKind {
  override def token = "Literal"

  override def check(node: RDFNode): Checker[RDFNode, ValidationError] =
    if (node.isLiteral) {
      checkFacets(node, facets)
    } else {
      // TODO...pass facets to error message
      err(MsgError(s"LiteralKind failed: node: $node is not a Literal or doesn't pass facets"))
    }
}

case class NonLiteralKind(
    shapeConstr: Option[ShapeConstr],
    facets: List[XSFacet]) extends NodeKind {
  override def token = "NonLiteral"

  override def check(node: RDFNode): Checker[RDFNode, ValidationError] =
    if (shapeConstr.isDefined)
      err(MsgError(s"IRIKind: $this, unimplemented check for shapeConstr. Node: $node"))
    else {
      if (!node.isLiteral)
        checkFacets(node, facets)
      else {
        // TODO...pass facets to error message
        err(MsgError(s"NonLiteralKind failed: node: $node is a Literal or doesn't pass facets"))
      }
    }
}

/**
 * ShapeConstr ::= SingleShape | NotShape | DisjShapeConstr | ConjShapeConstr
 */
sealed trait ShapeConstr extends ValueClass
  with Positional

case class SingleShape(shape: Label) extends ShapeConstr
case class NotShape(shape: Label) extends ShapeConstr
case class ConjShapeConstr(shapes: Seq[Label]) extends ShapeConstr
case class DisjShapeConstr(shapes: List[Label]) extends ShapeConstr
  