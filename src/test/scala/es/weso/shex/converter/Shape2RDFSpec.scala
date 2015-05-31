package es.weso.shex.converter

import org.scalatest._
import org.scalatest.prop._
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import es.weso.rdf.jena.RDFAsJenaModel
import com.hp.hpl.jena.rdf.model.Model
import es.weso.shex._
import util._

class Shape2RDFSpec
    extends FunSpec
    with Matchers
    with Checkers {

  describe("Shape2RDF") {

    it("Should be able to convert a shape in SHEXC to RDF") {
      val shape_str = """|prefix : <http://e.o#>
                         |:s { :a IRI }""".stripMargin

      val rdf_str = """|@prefix : <http://e.o#> .
                            |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                            |_:1 a sh:Schema .
                            |
                            |:s a sh:Shape ;
                            |   sh:schema _:1 ;
                            |   sh:property [ sh:predicate :a ; sh:valueType sh:IRI ] .
                            |""".stripMargin
      val r = for {
        (shape, _) <- Schema.fromString(shape_str, "SHEXC")
        rdf_from_shape <- RDFAsJenaModel.fromChars(shape.serialize("TURTLE"), "TURTLE")
        rdf_from_str <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
      } yield (rdf_from_shape, rdf_from_str)

      r match {
        case Success((r1, r2)) => shouldBeIsomorphic(r1, r2)
        case Failure(e) => fail("Exception parsing contents: " + e)
      }
    }
  }

  def shouldBeIsomorphic(r1: RDFAsJenaModel, r2: RDFAsJenaModel): Unit = {
    val b = r1.model.isIsomorphicWith(r2.model)
    if (!b) {
      println("Models are not isomorphic")
      println("-------------- Model 1:" + r1.serialize("TURTLE"))
      println("-------------- Model 2:" + r2.serialize("TURTLE"))
    }
    b should be(true)
  }

}