package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph._
import es.weso.parser.PrefixMap
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import com.hp.hpl.jena.query.Query

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 * */
object Shape2SPARQL {
  
  def shape2Sparql(schema: ShEx): Query = {
    
    ???
  } 
  
  def rule2Sparql(rule:Rule): Query = ???

}