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

class ShapeValidatorBackSpec 
	extends FunSpec
	with Matchers 
	with Checkers {
   /*
   it("should not validate (a?(b|c)) - Abc") {
     val ex = "http://example.org/"
     val strShape = """prefix :     <http://example.org/>
                      |prefix xsd:  <http://www.w3.org/2001/XMLSchema#>
                      |:Item { :a xsd:integer ? ,
                      |( :b xsd:integer
	                  || :c xsd:integer 
	                  |)
                      |}""".stripMargin('|')
 
     val strRDF = """prefix : <http://example.org/>
                    |:item :a "no match" ;
                    |      :b 1 ;
	                |      :c 1 .""".stripMargin('|') 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val matcher = Matcher(schema,rdf,false,false,ShapeValidatorBacktracking)
     val result = matcher.matchIRI_Label(IRI(ex+"item"))(IRILabel(IRI(ex+"Item")))
     info("Result: " + result)
     result.isValid should be(false)
   } */

   it("should validate open (b|c) - bc") {
     val ex = "http://example.org/"
     val strShape = """prefix :     <http://example.org/>
                      |prefix xsd:  <http://www.w3.org/2001/XMLSchema#>
                      |:Item { :b xsd:integer | :c xsd:integer }""".stripMargin('|')
 
     val strRDF = """prefix : <http://example.org/>
                    |:item :b 1 ; :c 1 .""".stripMargin('|') 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val matcher = Matcher(schema,rdf,false,false,ShapeValidatorWithDeriv)
     val result = matcher.matchIRI_Label(IRI(ex+"item"))(IRILabel(IRI(ex+"Item")))
     info("Result: " + result)
     result.isValid should be(true)
   }
   it("should validate closed (b|c) - bc") {
     val ex = "http://example.org/"
     val strShape = """prefix :     <http://example.org/>
                      |prefix xsd:  <http://www.w3.org/2001/XMLSchema#>
                      |:Item [ :b xsd:integer | :c xsd:integer ]""".stripMargin('|')
 
     val strRDF = """prefix : <http://example.org/>
                    |:item :b 1 ; :c 1 .""".stripMargin('|') 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val matcher = Matcher(schema,rdf,false,false,ShapeValidatorWithDeriv)
     val result = matcher.matchIRI_Label(IRI(ex+"item"))(IRILabel(IRI(ex+"Item")))
     info("Result: " + result)
     result.isValid should be(false)
   }
 }
 
 
