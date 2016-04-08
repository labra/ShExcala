package integration

import org.scalatest._
import es.weso.shex.Schema
import es.weso.shex.ShaclMatcher
import es.weso.rdf.jena.RDFAsJenaModel
import util._
import es.weso.rdf.nodes._
import es.weso.shex.ShEx._
import es.weso.rdf.validator._
import es.weso.utils.Logging
import org.slf4j._
import org.apache.log4j._


class ValidateSingle 
  extends FunSpec
  with Matchers 
  with ValidTester 
  with Logging {
  
  BasicConfigurator.configure()
  
  describe("Single Test") {
    it("Should be valid single") {
      val strData =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |:x :a :x1 . # C1
           |:x :a :x2 . # C2
           |:x :a :x3 . # C1
           |:x :b :x4 . # C3
           |# <x> :b 1 .
           |:x1 :b :y1 .
           |:x2 :b :y2 .
           |:x3 :b :y3 .
           |:y1 :c 1 .
           |:y2 :d 2 .
           |:y3 :c 3 .
           |:z :b :x4 .""".stripMargin

      val strSchema =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S1> { :a @<T1>*, (:a @<T2>+ | :b xsd:integer), :b IRI }
           |<S> { :b IRI ~ "x4$" }
           |<T1> { :b @<T3> }
           |<T2> { :b @<T4> }
           |<T3> { :c . }
           |<T4> { :d . }
           |""".stripMargin

      println(s"Log level = $log")
      setInfo()
      shouldBeValid(strSchema, strData,"http://a.example/z","S")
    } 
    
  }
}