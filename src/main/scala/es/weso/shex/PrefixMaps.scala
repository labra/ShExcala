package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes.IRI

object PrefixMaps {
  lazy val rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  lazy val rdfs = "http://www.w3.org/2000/01/rdf-schema#"
  lazy val shex = "http://www.w3.org/2013/ShEx/ns#"
  lazy val xsd = "http://www.w3.org/2001/XMLSchema#"
  lazy val ex = "http://example.org/"

  lazy val commonMap =
    Map("xsd" -> IRI(xsd), "rdf" -> IRI(rdf), "rdfs" -> IRI(rdfs), "shex" -> IRI(shex)
    )

  lazy val commonShex = PrefixMap(pm = commonMap)
  lazy val example = PrefixMap(commonMap + ("ex" -> IRI(ex)))

}

