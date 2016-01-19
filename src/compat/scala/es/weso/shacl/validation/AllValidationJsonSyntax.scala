package es.weso.shacl.validation

import com.typesafe.config._
import java.io.File
import argonaut._, Argonaut._
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }
import scala.io._
import org.scalatest.FunSpec
import org.scalatest._
import es.weso.shacl._

class AllValidationJsonSyntax extends Driver {

  describe("Test JSON parser only for validations.") {
    val validations = getValidations(validationFolder) 
    for ((file, json) <- validations) {
      it(s"Should parse ${file.getName}") {
        val tryCnv = for {
          validationAST <- astFile(file)
        } yield (validationAST)
        tryCnv match {
          case TrySuccess(valAST) => {}
          case TryFailure(e) => fail("Failure: " + e)
        }
      }
    }
  }

  describe("Test JSON parser to do: JSON -> AST -> JSON = JSON") {
    val validations = getValidations(validationFolder) 
    for ((file, json) <- validations) {
      it(s"Should parse ${file.getName}") {
        val tryCnv = for {
          validationAST <- getValidationFromFile(file)
        } yield (validationAST,json)
        tryCnv match {
          case TrySuccess((valAST,json)) => {
            val jsongenerated = valAST.asJson
            jsonsEqual(jsongenerated, json)
          }
          case TryFailure(e) => fail("Failure: " + e)
        }
      }
    }
  }
}

