package es.weso.shex

import es.weso.rdfgraph.nodes.IRI

object PREFIXES {

  // TODO: Move this function to Wesin
  def add(iri: IRI, str: String): IRI = {
    IRI(iri.uri.toString + str)
  }

  lazy val foaf = IRI("http://xmlns.com/foaf/0.1/")
  lazy val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  lazy val sh = IRI("http://www.w3.org/ns/shacl/core#")
  lazy val owl = IRI("http://www.w3.org/2002/07/owl#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  lazy val sh_IRI = add(sh, "IRI")
  lazy val sh_NonIRI = add(sh, "NonIRI")
  lazy val sh_Literal = add(sh, "Literal")
  lazy val sh_NonLiteral = add(sh, "NonLiteral")
  lazy val sh_BNode = add(sh, "BNode")
  lazy val sh_NonBNode = add(sh, "NonBNode")
  lazy val sh_Any = add(sh, "Any")
  lazy val sh_Shape = add(sh, "Shape")
  lazy val rdf_langString = add(rdf, "langString")
  lazy val xsd_string = add(xsd, "string")
  lazy val xsd_integer = add(xsd, "integer")
  lazy val xsd_double = add(xsd, "double")
  lazy val rdf_type = add(rdf, "type")

  private val shMap: Map[String, IRI] =
    Map("sh" -> sh,
      "xsd" -> xsd
    )

}