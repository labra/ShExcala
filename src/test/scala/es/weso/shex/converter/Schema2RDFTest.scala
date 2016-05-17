package es.weso.shex.converter

import scala.util.{ Failure, Success }

import org.scalatest.{ FunSpecLike, Matchers }
import org.scalatest.prop.Checkers

import org.apache.jena.rdf.model.ModelFactory

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.Schema
import es.weso.utils.JenaUtils

class Schema2RDFTest extends FunSpecLike
    with Matchers
    with Checkers {

  describe("Schema2RDF") {

    it("Should convert a simple definition") {
      val schemaStr = """|prefix : <http://example.org/>
                     |:a { :b IRI }
                     |""".stripMargin

      val expectedStr = """|@prefix :      <http://example.org/> .
                          |@prefix sh:    <http://www.w3.org/ns/shacl#> .
                          |@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
                          |
                          |:a      a sh:Shape ;
                          |  sh:property  [ a             sh:PropertyConstraint ;
                          |  sh:maxCount   1 ;
                          |  sh:minCount   1 ;
                          |  sh:nodeKind   sh:IRI ;
                          |  sh:predicate  :b ] .""".stripMargin

      val rdf = RDFAsJenaModel(ModelFactory.createDefaultModel())
      val tryConvert = for {
        expectedModel <- JenaUtils.parseFromString(expectedStr)
        (schema, pm) <- Schema.fromString(schemaStr)
      } yield (expectedModel, schema)

      tryConvert match {
        case Success((expectedModel, schema)) => {
          val convertedModel = rdf.model
          if (convertedModel.isIsomorphicWith(expectedModel)) {
            println("Are isomorphic")
          } else {
            println("Models are not equivalent")
            println("Converted model")
            convertedModel.write(System.out, "TURTLE")
            println("Expected model")
            expectedModel.write(System.out, "TURTLE")
          }

        }
        case Failure(e) => fail(s"Error converting $e")
      }

    }

  }
}