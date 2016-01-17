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
        """|@prefix ex: <http://example.org/> .
           |@prefix foaf: <http://xmlns.com/foaf/0.1/> .
           |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
           |
           |ex:john foaf:name "John" ; 
           |        ex:role ex:Tester .
           |ex:tom  foaf:name "Tomas" ;  
           |        ex:experience ex:senior ;  
           |        ex:assignedIssue ex:issue1 .
           |ex:mary foaf:givenName "Maria"; 
           |        foaf:lastName "Smith" .
           |ex:client1 ex:clientNumber 1 .
           |ex:issue1  ex:reportedBy ex:mary ; 
           |           ex:reproducedBy ex:john ; 
           |           ex:reproducedBy ex:tom .""".stripMargin

      val strSchema =
        """|prefix ex: <http://example.org/>
           |prefix foaf: <http://xmlns.com/foaf/0.1/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<TesterShape> {
           | foaf:name xsd:string,
           | ex:role IRI
           |}
           |
           |<ProgrammerShape> {
           |  foaf:name xsd:string, 
           |  ex:experience [ex:senior ex:young], 
           |  ex:assignedIssue @<ConfirmedIssueShape> *
           |}
           |
           |<UserShape> {
           | ( foaf:givenName xsd:string, foaf:lastName xsd:string
           | | foaf:name xsd:string
           | )
           |}
           |
           |<ClientShape> { 
           | ex:clientNumber xsd:integer    
           |}
           |
           |<ConfirmedIssueShape> {
           |  ex:reportedBy @<UserShape>
           |, ex:reproducedBy @<TesterShape>+
           |, ex:reproducedBy @<ProgrammerShape>+
           |, ^ex:assignedIssue @<ProgrammerShape>
           |}""".stripMargin


      shouldBeValid(strSchema, strData,"http://example.org/issue1","ConfirmedIssueShape")
    } 
    
  }
}