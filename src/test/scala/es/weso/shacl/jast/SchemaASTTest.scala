package es.weso.shacl.jast

import es.weso.shacl._
import Label._
import es.weso.shacl.jast.AST._

import org.scalatest._
import org.scalatest.prop._
import es.weso.rdf._
import util._


class SchemaAST 
 extends Driver 
   with FunSpecLike
   with Matchers
   with Checkers {

  describe("SchemaAST") {

      it("Should convert empty schema") {
        val shaclSchema = SHACLSchema(None, Map(), Map(), None, Actions.empty)
        val schema = Schema(PrefixMap.empty, shaclSchema)
        val expected = SchemaAST.empty
        val tryCnv = for {
          ast <- Schema2AST.cnvSchema(schema)
        } yield ast
        tryCnv match {
          case Success(ast) => {
            ast should be(expected)
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
          SHACLSchema.empty.copy(
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