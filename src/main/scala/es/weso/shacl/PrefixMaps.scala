package es.weso.shacl

import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes.IRI
import PREFIXES._

object PrefixMaps {
  lazy val commonMap =
    Map("xsd" -> xsd, 
        "rdf" -> rdf, 
        "rdfs" -> rdfs, 
        "sh" -> sh
    )

  lazy val commonShacl = PrefixMap(pm = commonMap)

}

