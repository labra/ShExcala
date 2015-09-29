package es.weso.shacl

import es.weso.rdfgraph.nodes.IRI

object PREFIXES {

  // TODO: Move this function to Wesin
  def add(iri: IRI, str: String): IRI = {
    IRI(iri.uri.toString + str)
  }

  lazy val foaf = IRI("http://xmlns.com/foaf/0.1/")
  lazy val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  
  lazy val sh = IRI("http://www.w3.org/ns/shacl#")
//  lazy val sh = IRI("http://www.w3.org/ns/shacl/core#")
  
  lazy val owl = IRI("http://www.w3.org/2002/07/owl#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  lazy val sh_IRI = add(sh, "IRI")
  lazy val sh_NonIRI = add(sh, "NonIRI")
  lazy val sh_Literal = add(sh, "Literal")
  lazy val sh_NonLiteral = add(sh, "NonLiteral")
  lazy val sh_BNode = add(sh, "BNode")
  lazy val sh_NonBNode = add(sh, "NonBNode")
  lazy val sh_OpenShape = add(sh,"OpenShape")
  lazy val sh_ClosedShape = add(sh,"ClosedShape")
  lazy val sh_ShapeClass = add(sh,"ShapeClass")
  lazy val sh_Graph = add(sh,"Graph")
  lazy val sh_Any = add(sh, "Any")
  lazy val sh_Shape = add(sh, "Shape")
  lazy val sh_choice = add(sh, "choice")
  lazy val sh_minCount = add(sh,"minCount")
  lazy val sh_maxCount = add(sh,"maxCount")
  lazy val sh_allowedValue = add(sh,"allowedValue")
  lazy val sh_allowedValues = add(sh,"allowedValues")
  lazy val sh_property = add(sh, "property")
  lazy val sh_predicate = add(sh,"predicate")
  lazy val sh_inverseProperty = add(sh, "inverseProperty")
  lazy val sh_PropertyConstraint = add(sh,"PropertyConstraint")
  lazy val sh_InversePropertyConstraint = add(sh,"InversePropertyConstraint")
  lazy val sh_nodeKind = add(sh,"nodeKind")
  lazy val sh_someOf = add(sh,"someOf")
  lazy val sh_oneOf = add(sh,"oneOf")
  lazy val sh_schema = add(sh, "schema")
  lazy val sh_Schema = add(sh, "Schema")
  lazy val sh_datatype = add(sh, "datatype")
  lazy val sh_valueShape = add(sh, "valueShape")
  lazy val sh_start = add(sh, "start")
  lazy val sh_group = add(sh,"group")
  lazy val sh_nodeShape = add(sh,"nodeShape")
  lazy val sh_text = add(sh,"text")
  lazy val rdf_langString = add(rdf, "langString")
  lazy val xsd_string = add(xsd, "string")
  lazy val xsd_integer = add(xsd, "integer")
  lazy val xsd_double = add(xsd, "double")
  lazy val xsd_decimal = add(xsd, "decimal")
  lazy val rdf_type = add(rdf, "type")
  lazy val rdf_first = add(rdf, "first")
  lazy val rdf_rest = add(rdf, "rest")
  lazy val rdf_nil = add(rdf, "nil")
  
  lazy val builtinTypes = List(xsd_integer, xsd_double, xsd_string)


  private val shMap: Map[String, IRI] =
    Map("sh" -> sh,
      "xsd" -> xsd
    )

}