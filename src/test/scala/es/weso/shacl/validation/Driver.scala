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
import es.weso.shacl.validation.Validation._
import es.weso.utils.testUtils._
//import es.weso.shacl._

class Driver extends FunSpec with Matchers with TryValues with TestUtils {

  val conf: Config = ConfigFactory.load()
  val testsDir = conf.getString("shexSyntaxTestsFolder")
  val parsedSchemasDir = testsDir + "parsedSchemas"
  val schemasDir = testsDir + "schemas"
  val negativeSyntaxDir = testsDir + "negativeSyntax"

  def str2ValAST(str: String): Try[ValAST] = {
    Try{
      val parsed = Parse.decodeValidation[ValAST](str)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing : $str"),
        x => x)
    }
  }


  def file2AST(file: File): Try[ValAST] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8").mkString
      val parsed = Parse.decodeValidation[ValAST](contents)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }

/*  def getParsedSchemas(jsonSchemasDir: String): List[(File, Json)] = {
    parseFolderFiles(jsonSchemasDir, file2json)
  } */
  
  def astFile(file: File): Try[ValAST] = {
    trying("Reading AST", file2AST(file))
  }
 
}