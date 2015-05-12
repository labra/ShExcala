package es.weso.shacl.converter

import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util._
import es.weso.rdf._
import es.weso.shacl.Schema
import es.weso.shex.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap

object RDF2Schema {
  def rdf2Schema(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    val pm = rdf.getPrefixMap
    val schema: Schema = ???
    Success((schema, pm))
  }

  def collectShapes(rdf: RDFReader): Set[RDFTriple] = {
    rdf.triplesWithPredicateObject(rdf_type, sh_Shape)
  }
}

