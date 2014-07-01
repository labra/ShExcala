package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.shex.ShapeValidator._
import es.weso.shex.Typing._
import es.weso.shex.Context._
import es.weso.parser.PrefixMap
import es.weso.rdf.RDFTriples

class ShapeValidatorAny
	extends FunSpec 
	with Matchers 
	with Checkers {
  
describe("Schema all") {
 it("Should validate all nodes ok when all pass") {
  val rdf_str = "@prefix : <http://example.org/> .\n" +
                ":n1 :p :b . \n" +
                ":n3 :p :d ."
  val rdf = RDFTriples.parse(rdf_str).get                
  val schema_str = "prefix : <http://example.org/> \n" +
                  ":a { :p . } "
  val schema = Schema.fromString(schema_str).get._1
  val rs = Schema.matchAll(rdf,schema)
  def ex(n:String) = IRI("http://example.org/" + n)
  val t0 = Typing.emptyTyping
  val t1 = t0.addType(ex("n1"),ex("a")).get
  val t2 = t1.addType(ex("n3"),ex("a")).get
  rs.run should be(Stream(t2))
 }

 it("Should validate some nodes ok when some pass") {
  val rdf_str = "@prefix : <http://example.org/> .\n" +
                ":n1 :p :b . \n" +
                ":n2 :q :b . \n" +
                ":n3 :p :d ."
  val rdf = RDFTriples.parse(rdf_str).get                
  val schema_str = "prefix : <http://example.org/> \n" +
                  ":a { :p . } "
  val schema = Schema.fromString(schema_str).get._1
  val rs = Schema.matchAll(rdf,schema)
  def ex(n:String) = IRI("http://example.org/" + n)
  val t0 = Typing.emptyTyping
  val t1 = t0.addType(ex("n1"),ex("a")).get
  val t2 = t1.addType(ex("n3"),ex("a")).get
  rs.run should be(Stream(t2))
 }

}
    
}
 
 
