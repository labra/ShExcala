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

object Schema2RDF {
  def toRDF(schema: Schema, rdf: RDFBuilder): RDFBuilder = {
    rdf.addPrefixMap(schema.pm)
    shapes2RDF(schema.shEx, rdf)
  }

  def shapes2RDF(shex: ShEx, current: RDFBuilder): RDFBuilder = {
    val shapeNode = current.createBNode
    current.addTriples(Set(RDFTriple(shapeNode, rdf_type, sh_Shape)))
  }

  def apply(schema: Schema): RDFBuilder = {
    toRDF(schema, RDFAsJenaModel.empty)
  }

}

