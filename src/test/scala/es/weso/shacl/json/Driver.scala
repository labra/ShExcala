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

class Driver extends FunSpec with Matchers with TryValues {

  val conf: Config = ConfigFactory.load()
  val testsDir = conf.getString("shexSyntaxTestsFolder")
  val parsedSchemasDir = testsDir + "parsedSchemas"
  val schemasDir = testsDir + "schemas"
  val negativeSyntaxDir = testsDir + "negativeSyntax"

  // method borrowed from: Alvin Alexander's Scala cookbook 
  def getFilesFromFolder(path: String): List[(File)] = {
    val d = new File(path)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def file2json(file: File): Try[Json] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8")
      val parsed = JsonParser.parse(contents.mkString)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }

  def file2AST(file: File): Try[SchemaAST] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8").mkString
      val parsed = Parse.decodeValidation[SchemaAST](contents)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }

  def parseFolderFiles[A](
    folder: String,
    parser: File => Try[A]): List[(File, A)] = {
    val files = getFilesFromFolder(folder)
    files.map(file => {
      val t = parser(file)
      if (t.isFailure) {
        println(s"Failure with file ${file.getName}: $t")
      }
      t.map(a => (file, a))
    }).filter(_.isSuccess).map(_.get)
  }

  def getParsedSchemas(jsonSchemasDir: String): List[(File, Json)] = {
    parseFolderFiles(jsonSchemasDir, file2json)
  }
  
  def nameWithoutExtension(file: File): String = {
    val name = file.getName
    if (name.contains('.')) 
        name.substring(0,name.lastIndexOf('.'))
    else
      name
  }
  
  def lookupFileWithSameName(file:File, otherPath: String, extension: String): Try[File] = {
    Try {
      val newFileName = otherPath + "/" + nameWithoutExtension(file) + "." + extension
      val d = new File(newFileName)
      if (d.exists && d.isFile) d
      else throw new Exception(s"File: $newFileName not found")
    }
  }
  
  def parseShaclSchema(file: File): Try[Schema] = for {
      (schema,prefixMap) <- Schema.fromString(io.Source.fromFile(file)("UTF-8").mkString)
  } yield schema
  
  def schemasEqual(fromast: Schema, parsed:Schema): Unit = {
    if (fromast == parsed) {
      info("Schemas are equal")
    } else {
      fail(s"Schemas different\nFrom AST:${fromast.showShapes}\nParsed  :${parsed.showShapes}")
    }
  }
  
  def trying[A](msg: String, t: Try[A]): Try[A] = {
    t match {
      case TrySuccess(v) => t
      case TryFailure(e) => {
        info(s"Failing $msg: ${e.toString}" )
        t
      }
    }
  }
  
  def astFile(file: File): Try[SchemaAST] = {
    trying("Reading AST", file2AST(file))
  }
  
  def astSchema(ast: SchemaAST): Try[Schema] = {
    trying("AST->Schema", AST2Schema.cnvAST(ast))
  }
  
  def testFile(file: File): Unit = {
    val tryConversion = for {
          schemaAST <- astFile(file)
          schema <- astSchema(schemaAST)
          shaclFile <- lookupFileWithSameName(file,schemasDir,"shex")
          shacl <- trying("Parsing SHACL", parseShaclSchema(shaclFile))
        } yield (schemaAST,schema,shacl)
     tryConversion match {
          case TrySuccess((schemaAST,schema, shacl)) => {
           schemasEqual(schema,shacl) 
          }
          case TryFailure(e)      => fail("Failure: " + e)
        }
  }
 
}