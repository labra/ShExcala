package es.weso.shex.converter

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import es.weso.rdf._
import es.weso.shex._
import es.weso.rdf.jena._
import es.weso.rdf.jena.RDFAsJenaModel

class Schema2RDF(rdf: RDFBuilder) {
  def schema2RDF(schema: Schema): RDFBuilder = {
    rdf.addPrefixMap(schema.pm)
    shapes2RDF(schema.shex, rdf)
  }

  def shapes2RDF(shex: ShEx, current: RDFBuilder): RDFBuilder = {
    ???
  }

}

object Schema2RDF {

}

