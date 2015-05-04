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

case class Schema2RDF(rdf:RDFBuilder) extends RDFBuilder {
  
  def toRDF(schema: Schema): Schema2RDF = {
    rdf.addPrefixMap(schema.pm)
    schema2RDF(schema.shEx, rdf)
  }

  def schema2RDF(shex: ShEx, current: RDFBuilder): RDFBuilder = {
    val schemaNode = current.createBNode
    addTriple(current, RDFTriple(schemaNode, rdf_type, sh_Shape))
    rules2RDF(current,shex.rules,schemaNode)
    start2RDF(cu)
  }

  def apply(schema: Schema): RDFBuilder = {
    toRDF(schema, RDFAsJenaModel.empty)
  }

  def addTriple(rdf: RDFBuilder, triple:RDFTriple): RDFBuilder = {
    rdf.addTriples(Set(triple))
  }
  
}

