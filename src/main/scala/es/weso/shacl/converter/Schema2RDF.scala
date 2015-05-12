package es.weso.shacl.converter

import es.weso.shacl._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import es.weso.rdf._
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
    shaclSchema2RDF(schema.shaclSchema, rdf)
    rdf
  }

  def shaclSchema2RDF(shaclSchema: SHACLSchema, rdf: RDFBuilder): RDFBuilder = {
    val (schemaNode, _) = rdf.createBNode
    rdf.addTriple(RDFTriple(schemaNode, rdf_type, sh_Schema))
    rules2RDF(shaclSchema.rules, schemaNode, rdf)
    //    start2RDF(shex.start, rdf)
  }

  def rules2RDF(
    rules: Set[Rule],
    schemaNode: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    for (rule <- rules) {
      rule2RDF(rule, schemaNode, rdf)
    }
    rdf
  }

  def rule2RDF(
    rule: Rule,
    schemaNode: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    val labelNode = rule.label.getNode
    rdf.addTriple(RDFTriple(labelNode, rdf_type, sh_Shape))
    rdf.addTriple(RDFTriple(labelNode, sh_schema, schemaNode))
    //    rule2RDF(rule.rule, labelNode, rdf)
  }

  /*
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
      case PlusRule(r) => ???
      case OptRule(r) => ???
      case StarRule(r) => ???
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
      case _ => ???
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
      case ValueAny(excl) => ???
      case ValueStem(s) => ???
      case ValueReference(l) => rdf.addTriple(RDFTriple(arcNode, sh_valueShape, l.getNode))
    }

  }

  def valueObject2RDF(vo: ValueObject, arcNode: RDFNode, rdf: RDFBuilder): RDFBuilder = {
    vo match {
      case RDFNodeObject(node) => rdf.addTriple(RDFTriple(arcNode, sh_allowedValue, node))
      case _ => ???
    }
  }

  // TODO: Add start declaration
  def start2RDF(label: Option[Label], rdf: RDFBuilder): RDFBuilder = {
    rdf
  }
*/
}