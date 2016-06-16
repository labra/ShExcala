package es.weso.shex

import es.weso.rdf.jena.RDFAsJenaModel
import org.scalatest._
import es.weso.rdf.nodes.{IRI, _}
import es.weso.validating._
import es.weso.shex.Label._



class XSFacetTest
  extends FunSpec
    with Matchers
    with TryValues {

  describe("XSFacet") {
    it("Should check MinInclusive") {
        val strData =
          """|@prefix : <http://example.org/> .
            |:x :p 5 .
            |""".stripMargin

        val strSchema =
          """|prefix : <http://example.org/>
            |<S> { :p . }
            |""".stripMargin

        val strNode = "http://example.org/x"
        val strLabel = "S"
        val result = for {
          (schema,pm) <- Schema.fromString(strSchema,"SHEXC",Some(""))
          data <- RDFAsJenaModel.fromChars(strData,"TURTLE",Some(""))
          ts <- schema.matchNode_Label(IRI(strNode),labelStr(strLabel),data)
        } yield ts

        print("Validation: " + result)
    }
  }
}
