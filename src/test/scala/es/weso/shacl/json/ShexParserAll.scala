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

class ShexParserAll extends Driver {

  describe("Run specific JSON test") {
    val name = "open3groupdotclose"
    val file = new File(parsedSchemasDir + "/" + name + ".json")
    it(s"should pass file $name") {
      testComparingJsons(file)
    }
  }

  describe("Test Shex parser:JSON -> AST -> Schema = SheXC -> Schema") {
    val parsedSchemas = getParsedSchemas(parsedSchemasDir)
    for ((file, json) <- parsedSchemas) {
      it(s"Should handle ${file.getName}") {
        testComparingSchemas(file)
      }
    }
  }


  describe("Negative JSON tests (parser should complain...)") {
    val negativeSyntaxFiles = getFilesFromFolder(negativeSyntaxDir)
    for (file <- negativeSyntaxFiles) {
      it(s"Should fail to parse ${file.getName}") {
        val t = Try {
          parseShaclSchema(file)
        }
        if (t.isSuccess) fail(s"Parsed ok. Result: ${t.get}")
        //        else info("Failed to parse as expected")
      }
    }
  }

}


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

}

