package es.weso.shex.converter

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util._
import es.weso.rdf._
import es.weso.shex.Schema
import es.weso.shex.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap

object RDF2Schema {
  def rdf2Schema(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    val pm = rdf.getPrefixMap
    val schema: Schema = throw new Exception("ShEx: RDF2Schema not implemented")
    Success((schema, pm))
  }

  def collectShapes(rdf: RDFReader): Set[RDFTriple] = {
    rdf.triplesWithPredicateObject(rdf_type, sh_Shape)
  }
}

