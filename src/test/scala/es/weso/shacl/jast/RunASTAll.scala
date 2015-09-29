package es.weso.shacl.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }
import scala.io._
import org.scalatest.FunSpec
import org.scalatest._
import es.weso.shacl.jast.AST._
import es.weso.shacl._

class RunASTAll extends Driver {

  describe("Test JSON parser only. JSON -> AST -> Schema") {
    val parsedSchemas = getParsedSchemas(parsedSchemasDir)
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
    val parsedSchemas = getParsedSchemas(parsedSchemasDir)
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

