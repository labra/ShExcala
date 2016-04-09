package es.weso.shex.jast

import es.weso.shex._
import Label._
import es.weso.shex.jast.AST._

import org.scalatest._
import org.scalatest.prop._

import es.weso.rdf._
import util._
import io._
import argonaut._, Argonaut._


class AST_SchemaTest 
 extends Driver 
   with FunSpecLike
   with Matchers
   with Checkers {

  describe("ASTSchema") {

      it("Should convert empty schema") {
        val ast = SchemaAST.empty
        
        val shaclSchema = ShExSchema(None, Map(), Map(), None, Actions.empty)
        val expected = Schema(PrefixMap.empty, shaclSchema)
        val tryCnv = for {
          schema <- astSchema(ast)
        } yield schema
        tryCnv match {
          case Success(schema) => {
            schemasEqual(schema,expected)
          }
          case Failure(e) => fail(s"Failing converting schema $e")
        }
    }
  
    it("Should convert schema with empty Shape") {
      val s1 = "http://example.org/S1"
        val shape = ShapeAST.empty
        val ast = SchemaAST.empty.copy(
            shapes = Some(Map(s1 -> shape))
        )
        
        val shaclSchema = 
          ShExSchema.empty.copy(
              shapes = Map(labelStr(s1) -> Shape.empty)
          )
        val expected = Schema(PrefixMap.empty, shaclSchema)
        val tryCnv = for {
          schema <- astSchema(ast)
        } yield schema
        tryCnv match {
          case Success(schema) => {
            schemasEqual(schema,expected)
          }
          case Failure(e) => fail(s"Failing converting schema $e")
        }
       }

    }
  

}