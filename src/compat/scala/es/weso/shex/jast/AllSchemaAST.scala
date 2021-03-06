package es.weso.shex.jast

import com.typesafe.config._
import argonaut._
import argonaut.Argonaut._
import scala.util.{Success => TrySuccess, Failure => TryFailure}
import scala.io._
import org.scalatest._
import es.weso.shex._
import es.weso.shex.jast.AST._
import scala.util.{Failure => TryFailure}
import scala.util.{Success => TrySuccess}

class AllSchemaAST extends Driver {

  describe("Test JSON parser only. JSON -> AST -> Schema") {
    val parsedSchemas = getParsedSchemas(schemasFolder)
    for ((file, json) <- parsedSchemas) {
      it(s"Should parse ${file.getName}") {
        val tryCnv = for {
          schemaAST <- astFile(file)
          schema <- astSchema(schemaAST)
        } yield (schemaAST, schema)
        tryCnv match {
          case TrySuccess((schemaAST, schema)) => {}
          case TryFailure(e)                   => fail("Failure: " + e)
        }
      }
    }
  }

  describe("Test JSON parser only. JSON -> AST -> JSON = JSON") {
    val parsedSchemas = getParsedSchemas(schemasFolder)
    for ((file, json) <- parsedSchemas) {
      it(s"Should parse ${file.getName}") {
        val tryCnv = for {
          ast <- astFile(file)
        } yield (ast,json)
        tryCnv match {
          case TrySuccess((ast,json)) => {
            val jsongenerated = ast.asJson
            jsonsEqual(jsongenerated, json)
          }
          case TryFailure(e) => fail("Failure: " + e)
        }
      }
    }
  }
}

