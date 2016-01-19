package es.weso.shacl.jast

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
import es.weso.shacl.jast.AST._
import es.weso.shacl._

class ShExParserAll extends Driver {

  describe("Test Shex parser:JSON -> AST -> Schema = SheXC -> Schema") {
    val parsedSchemas = getParsedSchemas(schemasFolder)
    for ((file, json) <- parsedSchemas) {
      it(s"Should handle ${file.getName}") {
        testComparingSchemas(file)
      }
    }
  }


  describe("Negative JSON tests (parser should complain...)") {
    val negativeSyntaxFiles = getFilesFromFolder(negativeSyntaxFolder)
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
