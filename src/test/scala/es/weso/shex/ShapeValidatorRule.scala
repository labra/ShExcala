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
	with ShapeValidatorBacktracking
	with Matchers 
	with Checkers {
  

 describe("Shape Validator Rule") {
/*   it("Should validate optional with empty") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 	    "prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 	    ":Item { :a xsd:integer ? } "  
     val strRDF = "prefix : <http://example.org/>\n" 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val matcher = Matcher(schema,rdf,false,false)
     val result = matcher.matchIRI_AllLabels(IRI("http://example.org/a"))
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
     val matcher = Matcher(schema,rdf,false,false)
     val result = matcher.matchAllIRIs_AllLabels()
     info("Result:\n" + result.toList.toString)
     result.isValid should be(false)
   }
*/
   it("Should not validate range 2 4 with 4") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 	    "prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 	    ":Item [ :a xsd:integer {2,4} ] "  
     val strRDF = "prefix : <http://example.org/>\n" +
                  ":item :a 1, 2, 3, 4 ." 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val matcher = Matcher(schema,rdf,false,false)
     val result = matcher.matchAllIRIs_AllLabels()
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

 it("Should validate range 2 4 with 2") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 	    "prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 	    ":Item [ :a xsd:integer {2,4} ] "  
     val strRDF = "prefix : <http://example.org/>\n" +
                  ":item :a 1, 2 ." 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val matcher = Matcher(schema,rdf,false,false)
     val result = matcher.matchAllIRIs_AllLabels()
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }
 }  
}
 
 
