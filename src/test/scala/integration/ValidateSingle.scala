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
    it("Should not be valid triple with any") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p 1, 2 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> { :p (1) | :p (2)}
           |""".stripMargin

      shouldNotBeValid(strSchema, strData,"http://example.org/x","S")
    }
  }
}