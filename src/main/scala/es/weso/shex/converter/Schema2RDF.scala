package es.weso.shex.converter

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import es.weso.rdf._
import es.weso.shex._
import es.weso.rdf.jena._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.utils._

object Schema2RDF extends Logging {

  def schema2RDF(schema: Schema, rdf: RDFBuilder): RDFBuilder = {
    rdf.addPrefixMap(schema.pm)
    rdf.addPrefix("sh", sh.str)
    rdf.addPrefix("xsd", xsd.str)
    shEx2RDF(schema.shEx, rdf)
    rdf
  }

  def shEx2RDF(shex: ShEx, rdf: RDFBuilder): RDFBuilder = {
    val (schemaNode, _) = rdf.createBNode
    rdf.addTriple(RDFTriple(schemaNode, rdf_type, sh_Schema))
    shapes2RDF(shex.rules, schemaNode, rdf)
    start2RDF(shex.start, rdf)
  }

  def shapes2RDF(shapes: Seq[Shape], schemaNode: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    for (shape <- shapes) {
      shape2RDF(shape, schemaNode, rdf)
    }
    rdf
  }

  def shape2RDF(shape: Shape, schemaNode: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    val labelNode = shape.label.getNode
    rdf.addTriple(RDFTriple(labelNode, rdf_type, sh_Shape))
    rdf.addTriple(RDFTriple(labelNode, sh_schema, schemaNode))
    rule2RDF(shape.rule, labelNode, rdf)
  }

  def rule2RDF(rule: Rule, label: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    rule match {
      case AndRule(r1, r2) => {
        rule2RDF(r1, label, rdf)
        rule2RDF(r2, label, rdf)
      }
      case OrRule(r1, r2) => {
        val (choiceNode, _) = rdf.createBNode
        rdf.addTriple(RDFTriple(label, sh_choice, choiceNode))
        rule2RDF(r1, choiceNode, rdf)
        rule2RDF(r2, choiceNode, rdf)
      }
      case EmptyRule => { rdf }
      case OpenRule(r) => rule2RDF(r, label, rdf)
      case PlusRule(r) => throw new Exception("PlusRule not implemented")
      case OptRule(r) => throw new Exception("OptRule not implemented")
      case StarRule(r) => throw new Exception("StarRule not implemented")
      case ArcRule(id, n, v) => {
        val arcNode: RDFNode =
          id match {
            case None => rdf.createBNode._1
            case Some(label) => label.getNode()
          }
        rdf.addTriple(RDFTriple(label, sh_property, arcNode))
        name2RDF(n, arcNode, rdf)
        value2RDF(v, arcNode, rdf)
      }
    }
  }

  def name2RDF(n: NameClass, arcNode: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    n match {
      case NameTerm(t) => rdf.addTriple(RDFTriple(arcNode, sh_predicate, t))
      case _ => throw new Exception("name2RDF not implemented")
    }
  }

  def value2RDF(v: ValueClass, arcNode: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    v match {
      case ValueType(v) => rdf.addTriple(RDFTriple(arcNode, sh_valueType, v))
      case ValueSet(set) => {
        for (vo <- set) {
          valueObject2RDF(vo, arcNode, rdf)
        }
        rdf
      }
      case ValueAny(excl) => throw new Exception("ValueAny not implemented")
      case ValueStem(s) => throw new Exception("valueStem not implemented")
      case ValueReference(l) => rdf.addTriple(RDFTriple(arcNode, sh_valueShape, l.getNode))
    }

  }

  def valueObject2RDF(vo: ValueObject, arcNode: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    vo match {
      case RDFNodeObject(node) => rdf.addTriple(RDFTriple(arcNode, sh_allowedValue, node))
      case _ => throw new Exception("ValueObject2RDf not implemented")
    }
  }

  // TODO: Add start declaration
  def start2RDF(label: Option[Label], rdf: RDFBuilder): RDFBuilder = {
    rdf
  }

}