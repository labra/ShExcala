package es.weso.shex.converter

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import es.weso.rdf.RDF
import es.weso.shex.Schema
import es.weso.shex.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple

object RDF2Shape {
  def rdf2Shape(rdf: RDF): Try[Schema] = {
    ???
  }

  def collectShapes(rdf: RDF): Set[RDFTriple] = {
    rdf.triplesWithPredicateObject(rdf_type, sh_Shape)
  }
}

