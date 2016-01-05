package es.weso.shacl.converter


import org.scalatest._
import org.scalatest.prop._
import com.hp.hpl.jena.rdf.model._
import org.apache.jena.riot.RDFDataMgr
import java.io.StringReader
import org.apache.jena.riot.RDFLanguages
import es.weso.shacl.Schema
import es.weso.rdf.jena.RDFAsJenaModel

class Schema2RDFShaclTest extends FunSpecLike
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
                          
        val expectedModel = ModelFactory.createDefaultModel;
        val reader = new StringReader(expectedStr) ;
        RDFDataMgr.read(expectedModel, reader, null, RDFLanguages.TURTLE) ;
        
        val (schema,pm) = Schema.fromString(schemaStr).get
        val rdf = RDFAsJenaModel.empty
        Schema2RDF.schema2RDF(schema,rdf)
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

    }
}