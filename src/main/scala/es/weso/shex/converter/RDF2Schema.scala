package es.weso.shex.converter

import scala.util.{ Failure, Success, Try }

import org.slf4s.Logging

import es.weso.rdf.{ PrefixMap, RDFReader }
import es.weso.rdf.nodes.{ BNodeId, IRI, Literal, RDFNode }
import es.weso.rdf.parser.RDFParser
import es.weso.shex.{ Datatype, EmptyShape, GroupShape, Label }
import es.weso.shex.{ RangeCardinality, ShExSchema, Schema, Shape, ShapeConstr, ShapeExpr, SingleShape, TripleConstraint, UnboundedCardinalityFrom, ValueClass }
import es.weso.shex.{ ValueConstr, ValueIRI, ValueLiteral, ValueObject, ValueSet }
import es.weso.shex.Cardinality
import es.weso.shex.Cardinality.defaultCardinality
import es.weso.shex.Label.mkLabel
import es.weso.shex.NodeKind
import es.weso.shex.PREFIXES.{ sh_ClosedShape, sh_Shape, sh_ShapeClass, sh_datatype, sh_in, sh_maxCount, sh_minCount, sh_nodeKind, sh_predicate, sh_property, sh_valueShape }
import es.weso.shex.ValueClass.nodeKindfromIRI
import es.weso.shex.XSFacet.emptyFacets
import es.weso.utils.TryUtils.filterSuccess

case class RDF2SchemaException(msg: String)
  extends Exception(s"RDF2SchemaException: $msg")

/**
 * Conversion between SHACL RDF to Schema
 *  
 */
object RDF2Schema
    extends Logging
    with RDFParser {

  /**
   * Parses the content of an RDF and obtains a Schema and a PrefixMap 
   */
  def rdf2Schema(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    val pm = rdf.getPrefixMap // .addPrefix("sh",IRI("http://www.w3.org/ns/shacl#")) 
    log.info("RDF2Schema...pm = " + pm)
    for {
      schema <- {
        val shex = shexSchema(rdf)
        shex
      }
    } yield (Schema(pm, schema), pm)
  }

  private def shexSchema(rdf: RDFReader): Try[ShExSchema] = {
    // TODO: Check node sh_Graph

    val shape_nodes = subjectsWithType(sh_Shape, rdf).toSet
    val shapeClass_nodes = subjectsWithType(sh_ShapeClass, rdf).toSet
    val closedShape_nodes = subjectsWithType(sh_ClosedShape, rdf).toSet
    val with_sh_property_nodes = subjectsWithProperty(sh_property, rdf).toSet
    val shapeCandidates =
      (shape_nodes ++
        shapeClass_nodes ++
        closedShape_nodes ++
        with_sh_property_nodes).toSeq.distinct // Remove duplicates

    log.info("Shape candidates = " + shapeCandidates)
    val maybeRules: Seq[Try[(Label, Shape)]] =
      shapeCandidates.map {
        case node => {
          rule(node, rdf)
        }
      }
    for {
      shapes <- filterSuccess(maybeRules)
      // TODO: Parse Schema label (if any)
      // TODO: Parse start (if any)
    } yield {
      ShExSchema.empty.copy(shapes = shapes.toMap)
    }
  }

  private def rule: RDFParser[(Label, Shape)] = { (n, rdf) =>
    {
      for {
        shape <- shape(n, rdf)
        lbl <- {
          mkLabel(n) match {
            case None      => fail(s"Node $n cannot be a label")
            case Some(lbl) => Success(lbl)
          }
        }
      } yield {
        (lbl, shape)
      }
    }
  }

  // TODO: closed
  // TODO: virtual
  // TODO: extras
  // TODO: inherit
  // TODO: Actions
  private def shape: RDFParser[Shape] = { (n, rdf) =>
    for {
      //okTypes <- hasNoRDFType(sh_ClosedShape)(n,rdf)
      //if okTypes
      shape <- {
        shapeExpr(n, rdf)
      }
      incls <- inclPropSet(n, rdf)
    } yield Shape.empty.copy(
      shapeExpr = shape,
      inherit = incls)
  }

  private def inclPropSet: RDFParser[Seq[Label]] = { (n, rdf) =>
    // Todo: 
    Success(Seq())
  }

  /*  def closedShape: RDFParser[ClosedShape] = { (n,rdf) =>
    for {
      checkType <- hasRDFType(sh_ClosedShape)(n,rdf)
      if checkType
      shape <- shapeExpr(n,rdf)
    } yield ClosedShape(shape)
  } */

  private def shapeExpr: RDFParser[ShapeExpr] = { (n, rdf) =>
    objectsFromPredicate(sh_property)(n, rdf) match {
      case Success(ps) => {
        log.info("shapeExpr...sh_property = " + ps)
        ps.size match {
          // TODO: check ids
          case 0 => Success(EmptyShape())
          case 1 => {
            val shapeExpr = oneOf(Seq(tripleConstraint))(ps.head, rdf)
            log.info(s"ShapeExpr: $shapeExpr")
            shapeExpr
          }
          case _ => for {
            shapes <- group(shapeExpr, ps.toSeq)(n, rdf)
          } yield GroupShape(None, shapes)
        }
      }
      case f =>
        fail("Cannot get objects from predicate " + sh_property + " at node " + n)
    }
  }

  private def tripleConstraint: RDFParser[ShapeExpr] = { (n, rdf) =>
    for {
      iri <- iriFromPredicate(sh_predicate)(n, rdf)
      valueClass <- valueClass(n, rdf)
      card <- cardinality(n, rdf)
      lbl <- {
        mkLabel(n) match {
          case None      => fail(s"Node $n cannot be a label")
          case Some(lbl) => Success(lbl)
        }
      }
    } yield {
      val t = TripleConstraint.empty.copy(
        id = None, // Some(lbl),
        iri = iri,
        value = valueClass,
        card = card)
      log.info(s"TripleConstraint: $t")
      t
    }
  }

  private def valueClass: RDFParser[ValueClass] = {
    oneOf(Seq(valueConstr, shapeConstr))
  }

  private def valueConstr: RDFParser[ValueConstr] = {
    oneOf(Seq(literalDatatype, nodeKind, valueSet))
  }

  private def nodeKindFromNode(n: RDFNode): Try[NodeKind] = {
    n match {
      case iri: IRI => nodeKindfromIRI(iri)
      case _        => Failure(throw RDF2SchemaException("Nodekind Value must be an IRI"))
    }
  }

  private def shapeConstr: RDFParser[ShapeConstr] = {
    oneOf(Seq(singleShape)) //TODO: Add disjShape, conjShape, notShape...
  }

  private def singleShape: RDFParser[SingleShape] = { (n, rdf) =>
    for {
      labelNode <- {
        // log.info("looking for single Shape: " + n)
        val obj = objectFromPredicate(sh_valueShape)(n, rdf)
        // log.info("objectFromPredicate valueShape = " + obj)
        obj
      }
      label <- {
        mkLabel(labelNode) match {
          case None      => fail(s"Node $n cannot be a label")
          case Some(lbl) => Success(lbl)
        }
      }
    } yield SingleShape(label)
  }

  private def literalDatatype: RDFParser[Datatype] = { (n, rdf) =>
    for {
      dt <- {
        log.info("literalDatatype: looking for valueType: " + n)
        val obj = objectFromPredicate(sh_datatype)(n, rdf)
        log.info("objectFromPredicate valueType = " + obj)
        obj
      }
      // TODO: Parse facets and check errors
    } yield {
      if (dt.isIRI)
        Datatype(dt.toIRI, emptyFacets)
      else
        throw RDF2SchemaException(s"literalDatatype: datatype must be an IRI. node: $n,  rdf: $rdf, datatype: $dt")
    }
  }

  // TODO: fallback to (nodeKind AnyKind) if no valueClass is declared 
  private def nodeKind: RDFParser[NodeKind] = { (n, rdf) =>
    for {
      nk_iri <- objectFromPredicate(sh_nodeKind)(n, rdf)
      nk <- nodeKindFromNode(nk_iri.toIRI)
    } yield {
      log.info(s"Node kind: $nk")
      nk
    }
  }

  private def valueSet: RDFParser[ValueSet] = { (n, rdf) =>
    {
      for {
        shapes <- objectsFromPredicate(sh_in)(n, rdf)
        if !shapes.isEmpty
      } yield ValueSet(shapes.map(node2valueObject).toSeq)
    }
  }

  private def node2valueObject(n: RDFNode): ValueObject = {
    n match {
      case lit: Literal => ValueLiteral(lit)
      case iri: IRI     => ValueIRI(iri)
      case b: BNodeId   => throw RDF2SchemaException("node " + n + " can't be converted to value object")
    }
  }

  private def cardinality: RDFParser[Cardinality] = { (n, rdf) =>
    if (hasPredicateWithSubject(n, sh_maxCount, rdf)) {
      if (hasPredicateWithSubject(n, sh_minCount, rdf)) {
        rangeCardinality(n, rdf)
      } else {
        rangeCardinalityOnlyMax(n, rdf)
      }
    } else {
      if (hasPredicateWithSubject(n, sh_minCount, rdf)) {
        unboundedCardinalityFrom(n, rdf)
      } else {
        Success(defaultCardinality)
      }
    }
  }

  private def rangeCardinalityOnlyMax: RDFParser[RangeCardinality] = { (node, rdf) =>
    for {
      n <- integerLiteralForPredicate(sh_maxCount)(node, rdf); if (n >= 1)
    } yield RangeCardinality(1, n)
  }

  private def rangeCardinality: RDFParser[RangeCardinality] = { (node, rdf) =>
    for {
      m <- integerLiteralForPredicate(sh_minCount)(node, rdf)
      n <- integerLiteralForPredicate(sh_maxCount)(node, rdf)
    } yield RangeCardinality(m, n)
  }

  private def unboundedCardinalityFrom: RDFParser[UnboundedCardinalityFrom] = { (node, rdf) =>
    for {
      m <- integerLiteralForPredicate(sh_minCount)(node, rdf)
    } yield UnboundedCardinalityFrom(m)
  }

}

