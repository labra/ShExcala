package integration

import org.scalatest._
import es.weso.shacl.Schema
import es.weso.rdf.jena.RDFAsJenaModel
import util._
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Shacl._

class ValidateTest extends FunSpec with Matchers with ValidTester {
  describe("Integration tests") {
    it("Should validate triple with any") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p 1 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |<S> { :p . }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should validate triple with IRIs") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p :y , :z .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |<S> { :p IRI+ }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should validate single references") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p :y .
           |:y :p 1 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |<S> { :p @<T> }
           |<T> { :p . }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should not validate triple with IRIs if there is a number") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p :y , 1 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |<S> { :p IRI+ }
           |""".stripMargin

      shouldNotBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should not validate triple with (1) and (2) and only (1)") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p 1 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |<S> { :p [1], :p [2] }
           |""".stripMargin

      shouldNotBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should validate Iovka's example") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :a :x1 , :x2, :x3; :b 1 .
           |:x1 :b :y1 .
           |:x2 :b :y2 .
           |:x3 :b :y3 .
           |:y1 :c 1 .
           |:y2 :d 2 .
           |:y3 :c 3 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> { :a @<T1>*, (:a @<T2>+ | :b xsd:integer ), :b xsd:integer }
           |<T1> { :b @<T3> } 
           |<T2> { :b @<T4> }
           |<T3> { :c . }
           |<T4> { :d . }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }
  }

  describe("Single test") {

    it("Should not validate extra with an extra triple if no extra is declared") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :a 1 , 2, "hi", 1.2 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> { :a xsd:integer+, :a xsd:string }
           |""".stripMargin

      shouldNotBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should validate extra with an extra triple if extra is declared") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :a 1 , 2, "hi", 1.2 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> EXTRA :a { :a xsd:integer+, :a xsd:string }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }

    it("Should validate triple with any") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p :y1, :y2 .
           |:y1 :q 1 .
           |:y2 :q 2 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> { :p @<T>+ }
           |<T> { :q xsd:integer }
           |""".stripMargin

      shouldBeValid(strSchema, strData, "http://example.org/x", "S")
    }
    
     it("Should validate triple with any - combining all nodes") {
      val strData =
        """|@prefix : <http://example.org/> .
           |:x :p :y1, :y2 .
           |:y1 :q 1 .
           |:y2 :q 2 .
           |""".stripMargin

      val strSchema =
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |<S> { :p @<T>+ }
           |<T> { :q xsd:integer }
           |""".stripMargin

      shouldBeValidAllNodes(strSchema, strData)
    }
     
    it("Should be valid single with an EXTRA") {
      val strData =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |:x :a 1; :a 3; :b 2 . 
           |""".stripMargin

      val strSchema =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |<S> EXTRA :a { :a [1 2] }
           |""".stripMargin


      shouldBeValid(strSchema, strData,"http://a.example/x","S")
    }
 
    it("Should not be valid single with extra triples without EXTRA") {
      val strData =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |:x :a 1; :a 3; :b 2 . 
           |""".stripMargin

      val strSchema =
        """|PREFIX : <http://a.example/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |<S> { :a [1 2] }
           |""".stripMargin


      shouldNotBeValid(strSchema, strData,"http://a.example/x","S")
    }

   it("Should be valid example from paper") {
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