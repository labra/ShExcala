package integration

import org.scalatest._
import es.weso.shex.Schema
import es.weso.shex.ShExMatcher
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
     it("Should validate with an OR second") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p :x2; :q 1 .
           |:x2 :t2 1 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |<S> { :p (@<T1> OR @<T2>)?, :q . }
           |<T1> { :t1 . }
           |<T2> { :t2 . }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }
    
  }
}