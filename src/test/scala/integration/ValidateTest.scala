package integration

import org.scalatest._
import es.weso.shacl.Schema
import es.weso.rdf.jena.RDFAsJenaModel
import util._
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Shacl._

class ValidateTest extends FunSpec with Matchers {
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
           
      shouldBeValid(strSchema,strData,"http://example.org/x","S") 
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
           
      shouldBeValid(strSchema,strData,"http://example.org/x","S") 
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
           
      shouldBeValid(strSchema,strData,"http://example.org/x","S") 
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
           
      shouldNotBeValid(strSchema,strData,"http://example.org/x","S") 
    }
  }
  
  def shouldBeValid(strSchema: String, strData: String, strNode: String, strLabel: String): Unit = {
    val result = for {
      (schema,pm) <- Schema.fromString(strSchema)
      data <- RDFAsJenaModel.fromChars(strData,"TURTLE")
      ts <- schema.matchNode_Label(IRI(strNode),mkLabel(strLabel),data)
    } yield ts
    
    result match {
      case Failure(e) => fail(s"Exception: $e")
      case Success(ts) => 
        if (ts.isEmpty) fail("No results")
        else info(s"Validated with typings $ts")
    }
  }
    
  def shouldNotBeValid(strSchema: String, strData: String, strNode: String, strLabel: String): Unit = {
    val result = for {
      (schema,pm) <- Schema.fromString(strSchema)
      data <- RDFAsJenaModel.fromChars(strData,"TURTLE")
      ts <- schema.matchNode_Label(IRI(strNode),mkLabel(strLabel),data)
    } yield ts
    
    result match {
      case Success(ts) if (!ts.isEmpty) => fail(s"Validates with $ts but should fail")
      case _ => return
    }
     
  }
}