package es.weso.shacl.json

import com.typesafe.config._
import java.io.File
import argonaut._, Argonaut._
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }
import scala.io._
import scalaz.\/
import org.scalatest.FunSpec
import org.scalatest._
import scalaz._, Scalaz._
import es.weso.shacl.json.AST._
import es.weso.shacl._

class ASTJsonDecoder extends FunSpec with Matchers with TryValues {
  
  describe("AST<->JSON") {

    it("should parse simple valueClass with nodekind BNode") {
      val str = """|{ "type": "valueClass",
                   |  "nodeKind": "bnode",
                   |  "length": 20
                   |} """.stripMargin

      val vc = ValueClassAST.empty.copy(
        nodeKind = Some("bnode"),
        length = Some(20),
        maxInclusive = None)
      checkJsonDecoder(str, vc)
    }

    it("should parse empty valueClass") {
      val str = """|{ "type": "valueClass"
                   |} """.stripMargin

      val vc = ValueClassAST.empty
      checkJsonDecoder(str, vc)
    }

    it("should parse single tripleConstraint") {
      val str = """|{ "type": "tripleConstraint",
                   |  "predicate": "http://a.example/p1",
                   |  "value": { "type": "valueClass",
                   |  "nodeKind": "literal",
                   |  "maxinclusive": 5 }
                   |}""".stripMargin

      val vc = ExpressionAST.empty.copy(
        _type = "tripleConstraint",
        predicate = Some("http://a.example/p1"),
        value = Some(ValueClassAST.empty.copy(
          nodeKind = Some("literal"),
          maxInclusive = Some(5))))
          
      checkJsonDecoder(str, vc)
    }

    it("should parse list of prefixes") {
      val str = """|{ "prefixes" : 
                   |   { "p1": "prefix1",
                   |     "p2": "prefix2" },
                   |  "shapes": {}
                   |}""".stripMargin

      val expected =
        SchemaAST.empty.copy(
          prefixes = Some(Map("p1" -> "prefix1", "p2" -> "prefix2")),
          shapes = Some(Map()))
      checkJsonDecoder(str, expected)
    }

    it("should parse list of shapes") {
      val str = """|{ "prefixes": {},
                   |  "shapes" : 
                   |   { "http://a.example/IssueShape": {
                   |     "type": "shape",
                   |     "expression": {
                   |        "type": "tripleConstraint",
                   |        "predicate": "http://a.example/p1",        
                   |        "value": { "type": "valueClass" }
                   |       }
                   |     }
                   |   }
                   |}""".stripMargin

      val expected =
        SchemaAST.empty.copy(
          shapes = Some(Map("http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = Some("http://a.example/p1"),
                value = Some(ValueClassAST.empty)))))),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)
    }

    it("should parse inverse negated shapes") {
      val str = """|{
                   |  "type": "schema",
                   |    "prefixes": {},
                   |      "shapes":{
                   |          "http://a.example/IssueShape": {
                   |                "type": "shape",
                   |                "expression": {
                   |                   "type": "tripleConstraint",
                   |                   "inverse": true,
                   |                   "negated": true,
                   |                   "predicate": "http://a.example/p1",
                   |                   "value": { "type": "valueClass" }}}
                   |}}""".stripMargin
      val expected =
        SchemaAST.empty.copy(
          shapes = Some(Map("http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = Some("http://a.example/p1"),
                value = Some(ValueClassAST.empty),
                negated = Some(true),
                inverse = Some(true)))))),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)
    }
    
   it("should parse references") {
     val str = """{
  "type": "schema",
  "prefixes": {},
  "shapes": {
    "http://a.example/EmployeeShape": {
      "type": "shape",
      "expression": {
        "type": "tripleConstraint",
        "predicate": "http://a.example/p2",
        "value": { "type": "valueClass", "reference": "http://a.example/PersonShape" }
      }
    }
  }
}
"""
 val expected =
     SchemaAST.empty.copy(
       shapes = 
          Some(Map(
            "http://a.example/EmployeeShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = Some("http://a.example/p2"),
                value = Some(ValueClassAST.empty.copy(
                    reference=Some(ReferenceAST(Left("http://a.example/PersonShape")))
                  )
                ))))
           )),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)     
   }

  
 it("should parse groups") {
     val str = """{
  "type": "schema",
  "prefixes": {},
  "shapes": {
    "http://a.example/IssueShape": {
      "type": "shape",
      "expression": {
        "type": "group",
        "expressions": [
          {
            "type": "tripleConstraint",
            "predicate": "http://a.example/p1",
            "value": { "type": "valueClass" }
          }]
      }
    }
  }
}
"""
 val expected =
     SchemaAST.empty.copy(
       shapes = 
          Some(Map(
            "http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = 
                Some(ExpressionAST.empty.copy(
                _type = "group",
                expressions = Some(List(
                   ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = Some("http://a.example/p1"),
                value = Some(ValueClassAST.empty)
            )))
        ))))),
       prefixes = Some(Map()))
      checkJsonDecoder(str, expected)     
   }

 it("should parse expression group") {
     val str = """{
      "type": "shape",
      "expression": {
        "type": "group",
        "expressions": [
          {
            "type": "tripleConstraint",
            "predicate": "http://a.example/p1",
            "value": { "type": "valueClass" }
          }]
      }
    }
"""
 val expected =
     ShapeAST.empty.copy(
              expression = 
                Some(ExpressionAST.empty.copy(
                _type = "group",
                expressions = Some(List(
                   ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = Some("http://a.example/p1"),
                value = Some(ValueClassAST.empty)
            )))
        )))
 checkJsonDecoder(str, expected)     
}

  }
  

  def checkJsonDecoder[A: DecodeJson](
    str: String,
    expected: A): Unit = {
    val parsed = str.decodeValidation[A]
    parsed match {
      case Success(v) => v should be(expected)
      case Failure(e) => { fail(e) }
    }
  }

}