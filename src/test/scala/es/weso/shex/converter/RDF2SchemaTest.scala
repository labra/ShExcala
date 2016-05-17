package es.weso.shex.converter


import org.scalatest._
import org.scalatest.prop._

import org.apache.jena.rdf.model._

import org.apache.jena.riot.RDFDataMgr

import java.io.StringReader

import org.apache.jena.riot.RDFLanguages

import es.weso.shex.Schema
import es.weso.rdf.jena.RDFAsJenaModel
import util._

class RDF2SchemaTest extends FunSpecLike
    with Matchers
    with Checkers {

  describe("RDF2Schema") {

      it("Should convert a simple definition") {
       val expectedShapeStr = """|prefix : <http://example.org/>
                                 |prefix sh: <http://www.w3.org/ns/shacl#>
                                 |:a { :b IRI }
                                 |""".stripMargin

        val rdfStr = """|@prefix :      <http://example.org/> .
                          |@prefix sh:    <http://www.w3.org/ns/shacl#> .
                          |
                          |:a      a sh:Shape ;
                          |  sh:property  [ a  sh:PropertyConstraint ;
                          |  sh:maxCount   1 ;
                          |  sh:minCount   1 ;
                          |  sh:nodeKind   sh:IRI ;
                          |  sh:predicate  :b ] .""".stripMargin
                       
        val tryConversion = for {
          rdf <- RDFAsJenaModel.fromChars(rdfStr,"TURTLE")
          (expected,pm1) <- Schema.fromString(expectedShapeStr)
          (schema,pm2) <- RDF2Schema.rdf2Schema(rdf)
        } yield(schema,expected)
        tryConversion match {
          case Success((schema,expected)) => {
            info("schema -> " + schema)
            info("expected -> " + expected)
            schema should be(expected)
          }
          case Failure(e) => fail(s"Error converting. $e") 
        }
      }

    }
}