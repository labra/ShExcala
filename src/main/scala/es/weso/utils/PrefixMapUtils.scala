package es.weso.utils

import es.weso.rdfgraph.nodes._
import es.weso.rdf.PrefixMap

object PrefixMapUtils {
  
    def qualify(iri: IRI)(implicit pm:PrefixMap): String = {
    val map = pm.pm
    val candidates = 
      map.filter{ 
        case (k,v) => iri.str.contains(v.str) 
      }.map { 
        case (k,v) => (k, iri.str.stripPrefix(v.str))
      }

    if (candidates.size > 0) { 
      val (prefix,suffix) = candidates.head 
      prefix + ":" + suffix  
    } else 
      iri.str
  }

}