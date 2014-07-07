package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.shex.Typing._
import es.weso.shex.Context._
import es.weso.parser.PrefixMap
import es.weso.rdf.RDFTriples

class ShapeValidatorRule 
	extends FunSpec
	with ShapeValidatorWithDeriv
	with Matchers 
	with Checkers {
  

 describe("Shape Validator Rule") {
   it("Should validate optional with empty") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 	    "prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 	    ":Item { :a xsd:integer ? } "  
     val strRDF = "prefix : <http://example.org/>\n" 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("http://example.org/a"),rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }
   
   it("Should not validate one or more with empty") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 	    "prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 	    ":Item { :a xsd:integer + } "  
     val strRDF = "prefix : <http://example.org/>\n" 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchAll(rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(false)
   }

 }  
}
 
 
