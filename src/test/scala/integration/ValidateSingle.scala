package integration

import org.scalatest._
import es.weso.shacl.Schema
import es.weso.shacl.ShaclMatcher
import es.weso.rdf.jena.RDFAsJenaModel
import util._
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Shacl._
import es.weso.rdf.validator._

class ValidateSingle extends FunSpec with Matchers with ValidTester {
  describe("Single Test") {
    it("Should be valid single") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :a 1; :b 1; :c 1 ; :d 1 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> { (:a . ,:b .)+ , :c . } 
           |""".stripMargin

      shouldBeValid(strSchema, strData,"http://example.org/x","S")
    }
  }
}