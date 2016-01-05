package es.weso.shacl.validation

import com.typesafe.config._
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }
import scala.io._
import org.scalatest.FunSpec
import org.scalatest._
import es.weso.shacl._
import argonaut._, Argonaut._

class RunValASTSingle extends Driver {

  describe("Test JSON parser to do: JSON -> AST -> JSON = JSON") {
 
    val name = "1integerMininclusiveDECIMALLeadTrail_pass-integer-equal"
    val file = new File(validationFolder + "/" + name + ".val")
    val contents = io.Source.fromFile(file)("UTF-8").mkString
    val json = Parse.parse(contents).getOrElse(jEmptyObject)
    
    val validations = Seq((file,json))
    for ((file, json) <- validations) {
      it(s"Should parse ${file.getName}") {
        val tryCnv = for {
          validationAST <- getValidationFromFile(file)
        } yield (validationAST,json)
        tryCnv match {
          case TrySuccess((valAST,json)) => {
            println("ValAST: " + valAST)
            val jsongenerated = valAST.asJson
            jsonsEqual(jsongenerated, json,true)
          }
          case TryFailure(e) => fail("Failure: " + e)
        }
      }
    }
  }
}

