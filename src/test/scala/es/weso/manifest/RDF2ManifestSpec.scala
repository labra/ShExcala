package es.weso.manifest

import org.scalatest._
import org.scalatest.prop._
import es.weso.rdfgraph.nodes._
import es.weso.manifest.ManifestPrefixes._
import es.weso.rdf.jena.RDFAsJenaModel

class RDF2ManifestSuite extends RDF2Manifest
    with FunSpecLike
    with Matchers
    with Checkers
    with TryValues {

  describe("RDF2Manifest") {
    describe("valueClass") {

      it("Should parse a simple Manifest file") {
        val str = """|@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                     |@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .
                     |@prefix mf:      <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#> .
                     |@prefix qt:      <http://www.w3.org/2001/sw/DataAccess/tests/test-query#> .
                     |@prefix sh:      <http://www.w3.org/ns/shacl#> .
                     |@prefix sht:     <http://www.w3.org/ns/shacl/test-suite#> .
                     |@prefix :        <http://example.org/> .
                     |:m a mf:Manifest ;
                     |   rdfs:label "Example" ;
                     |   mf:entries ( :entry1 ) .
                     |:entry1 a sht:Validate ;
                     |        mf:name "Validate1";
                     |        mf:action [ sht:schema :schema1 
                     |                  ; sht:data :data1
                     |                  ];
                     |        mf:result true ;
                     |        mf:status sht:proposed .""".stripMargin
                     
        val default = IRI("http://example.org/")
        val action = ManifestAction().
                     setSchema(default.add("schema1")).
                     setData(default.add("data1")) 
        val entry = 
          Entry(entryType = Validate,
                name = "Validate1",
                action = action,
                result = BooleanResult(true),
                status = Status(sht_proposed),
                specRef = None) 
        val expected = 
          Manifest(
              label = Some("Example"),
              comment = None,
              entries = List(entry),
              includes = List()
              )

        val result = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- rdf2Manifest(rdf,false)
        } yield v 
        
        result.success.value should be(Seq(expected))

      }
    }
  }
      
}