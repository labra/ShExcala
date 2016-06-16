package es.weso.shex.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }

import scala.io._

import org.scalatest.FunSpec
import org.scalatest._

import es.weso.shex.jast.AST._
import es.weso.shex._

class ShExDocAll extends Driver {
  
  describe("Compare as: JSON -> AST -> Schema -> Show -> ShExC -> Schema = Json -> AST -> Schema") {
    val parsedSchemas = getParsedSchemas(schemasFolder)
    if (parsedSchemas.isEmpty) {
      info(s"No schemas found in $schemasFolder") 
    } else
    for ((file, json) <- parsedSchemas) {
      it(s"Should parse ${file.getName} and convert it to doc") {
        val tryCnv = for {
          schemaAST <- astFile(file)
          schema <- astSchema(schemaAST)
          val schemaStr = schema.show 
        } yield {
          (schemaAST, schema,schemaStr)
        }
        tryCnv match {
          case TrySuccess((schemaAST, schema,schemaStr)) => {
            info(s"Parsing...\n$schemaStr")
            val tryParse = for {
              (schema2, _) <- Schema.fromString(schemaStr)
            } yield schema2
            tryParse match {
              case TrySuccess(schema2) => {
                schema should be(schema2)
              }
              case TryFailure(e) => {
                info(s"Parsing...\n$schemaStr")
                fail(s"Failed to parse: $e\n")
              }
            }
          }
          case TryFailure(e) => {
           fail("Failure reading schemas: " + e) 
          }
        }
      }
    }
  }
  
}