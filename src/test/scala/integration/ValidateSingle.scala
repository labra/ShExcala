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
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |BASE <http://a.example/>
           |<x> :a <x1> . # C1
           |<x> :a <x2> . # C2
           |<x> :a <x3> . # C1
           |<x> :b <x4> . # C3
           |# <x> :b 1 .
           |<x1> :b <y1> .
           |<x2> :b <y2> .
           |<x3> :b <y3> .
           |<y1> :c 1 .
           |<y2> :d 2 .
           |<y3> :c 3 .""".stripMargin

      val strSchema =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |<S> { :a @<T1>*, (:a @<T2>+ | :b xsd:integer), :b IRI ~ "^x4$" }
           |<T1> { :b @<T3> }
           |<T2> { :b @<T4> }
           |<T3> { :c . }
           |<T4> { :d . }""".stripMargin


      shouldBeValid(strSchema, strData,"http://a.example/x","S")
    } 
    
  }
}