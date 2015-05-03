package es.weso.shex.converter

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import es.weso.rdf._
import es.weso.shex.Schema
import es.weso.shex.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap

object RDF2Schema {
  def apply(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    ???
  }

  def collectShapes(rdf: RDF): Set[RDFTriple] = {
    rdf.triplesWithPredicateObject(rdf_type, sh_Shape)
  }
}

