package es.weso.shex.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

import scala.util._ 



import org.scalatest.FunSpec
import org.scalatest._

import es.weso.shex.jast.AST._
import es.weso.shex._

class ASTJsonDecoderTest extends FunSpec with Matchers with TryValues {
  
  describe("AST<->JSON") {

    it("should parse simple valueClass with nodekind BNode") {
      val str = """|{ "type": "ValueClass",
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
      val str = """|{ "type": "ValueClass"
                   |} """.stripMargin

      val vc = ValueClassAST.empty
      checkJsonDecoder(str, vc)
    }

    ignore("should parse single tripleConstraint") {
      val str = """|{ "type": "TripleConstraint",
                   |  "predicate": "http://a.example/p1",
                   |  "value": { "type": "ValueClass",
                   |  "nodeKind": "literal",
                   |  "maxinclusive": 5 }
                   |}""".stripMargin

      val vc = ValueClassAST.empty.copy(
          nodeKind = Some("literal"),
          maxInclusive = Some(NumberAST(Left(5)))
          )
      val tc = ExpressionAST.empty.copy(
        _type = Some("TripleConstraint"),
        predicate = Some("http://a.example/p1"),
        valueExpr = Some(vc))
          
      checkJsonDecoder(str, tc)
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

    ignore("should parse list of shapes") {
      val str = """|{ "prefixes": {},
                   |  "shapes" : 
                   |   { "http://a.example/IssueShape": {
                   |     "type": "shape",
                   |     "expression": {
                   |        "type": "TripleConstraint",
                   |        "predicate": "http://a.example/p1",        
                   |        "value": { "type": "ValueClass" }
                   |       }
                   |     }
                   |   }
                   |}""".stripMargin

      val expected =
        SchemaAST.empty.copy(
          shapes = Some(Map("http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = Some("TripleConstraint"),
                predicate = Some("http://a.example/p1"),
                valueExpr = Some(ValueClassAST.empty)))))),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)
    }

    ignore("should parse inverse negated shapes") {
      val str = """|{
                   |  "type": "schema",
                   |    "prefixes": {},
                   |      "shapes":{
                   |          "http://a.example/IssueShape": {
                   |                "type": "Shape",
                   |                "expression": {
                   |                   "type": "TripleConstraint",
                   |                   "inverse": true,
                   |                   "negated": true,
                   |                   "predicate": "http://a.example/p1",
                   |                   "value": { "type": "ValueClass" }}}
                   |}}""".stripMargin
      val expected =
        SchemaAST.empty.copy(
          shapes = Some(Map("http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = Some("TripleConstraint"),
                predicate = Some("http://a.example/p1"),
                valueExpr = Some(ValueClassAST.empty),
                negated = Some(true),
                inverse = Some(true)))))),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)
    }
    
   ignore("should parse references") {
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
                _type = Some("TripleConstraint"),
                predicate = Some("http://a.example/p2"),
                valueExpr = Some(ValueClassAST.empty.copy(
                    reference=Some(ReferenceAST(Left("http://a.example/PersonShape")))
                  )
                ))))
           )),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)     
   }

  
 ignore("should parse groups") {
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
                _type = Some("Group"),
                expressions = Some(List(
                   ExpressionAST.empty.copy(
                _type = Some("TripleConstraint"),
                predicate = Some("http://a.example/p1"),
                valueExpr = Some(ValueClassAST.empty)
            )))
        ))))),
       prefixes = Some(Map()))
      checkJsonDecoder(str, expected)     
   }

 ignore("should parse expression group") {
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
                _type = Some("Group"),
                expressions = Some(List(
                   ExpressionAST.empty.copy(
                _type = Some("TripleConstraint"),
                predicate = Some("http://a.example/p1"),
                valueExpr = Some(ValueClassAST.empty)
            )))
        )))
 checkJsonDecoder(str, expected)     
}

  

    ignore("should parse stems ") {
      val str = """|{ "type" : "valueClass",
                   |  "values": [
                   |{ "type": "stemRange",
                   |  "stem": { "type": "wildcard" },
                   |  "exclusions": [
                   |    { "type": "stem", "stem": "http://a.example/v1"},
                   |    { "type": "stem", "stem": "http://a.example/v2"}
                   | ] } ] }""".stripMargin
                   
      val v1 = "http://a.example/v1"             
      val v2 = "http://a.example/v2"             
      val e1 : ExclusionAST = ExclusionAST(Right(StemAST(Left(v1))))
      val e2 : ExclusionAST = ExclusionAST(Right(StemAST(Left(v2))))
        

      val vc = ValueClassAST.empty.copy(
        values = Some(List(ValueAST(Right(
              StemRangeAST(
                  _type=Some("StemRange"),
                  stem = Some(StemAST(Right(WildCardAST.empty))),
                  exclusions = Some(List(e1,e2))
                  )
            ))))
      )
        
      checkJsonDecoder(str, vc)
    }


    ignore("should parse exclusions stemRange") {
      val str = """|
                   |{ "type": "stemRange",
                   |  "stem": { "type": "wildcard" },
                   |  "exclusions": [
                   |    { "type": "stem", "stem": "http://a.example/v1"},
                   |    { "type": "stem", "stem": "http://a.example/v2"}
                   | ] }""".stripMargin
                   
      val v1 = "http://a.example/v1"             
      val v2 = "http://a.example/v2"             
      val e1 : ExclusionAST = ExclusionAST(Right(StemAST(Left(v1))))
      val e2 : ExclusionAST = ExclusionAST(Right(StemAST(Left(v2))))
        

      val vc = StemRangeAST(
          _type=Some("StemRange"),
                  stem = Some(StemAST(Right(WildCardAST.empty))),
                  exclusions = Some(List(e1,e2)
                )
                  
            
      )
        
      checkJsonDecoder(str, vc)
    }

    
    ignore("should parse exclusions") {
      val str = """| { "type": "stem", 
                   |   "stem": "http://a.example/v1"
                   | }""".stripMargin
                   
      val vc = ExclusionAST(Right(StemAST(Left("http://a.example/v1"))))
      checkJsonDecoder(str, vc)
    }

  }
  
  def checkJsonDecoder[A: DecodeJson](
    str: String,
    expected: A): Unit = {
    AST.parseGeneric[A](str) match {
      case Success(v) => {
        if (v === expected)
          info("Both are equal")
        else {
          fail(s"Values are different:\n$v\n${expected}")
        } 
      }
      case Failure(e) => { fail(e) }
    }
  }

}