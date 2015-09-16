package es.weso.shacl.validation

import com.typesafe.config._
import java.io.File
import argonaut._, Argonaut._
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }
import scala.io._
//import scalaz.\/
import org.scalatest.FunSpec
import org.scalatest._
//import scalaz._, Scalaz._
import es.weso.shacl.ast.AST._
import es.weso.shacl._

class RunValASTSingle extends Driver {

  describe("Test JSON parser to do: JSON -> AST -> JSON = JSON") {
 
    val name = "skipped"
    val file = new File(validationsDir + "/" + name + ".val")
    val contents = io.Source.fromFile(file)("UTF-8").mkString
    val json = Parse.parse(contents).getOrElse(jEmptyObject)
    
    val validations = Seq((file,json))
    for ((file, json) <- validations) {
      it(s"Should parse ${file.getName}") {
        val tryCnv = for {
          validationAST <- astFile(file)
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

