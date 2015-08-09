package es.weso.shacl.json
import com.typesafe.config._
import java.io.File
import argonaut._, Argonaut._
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.Try
import scala.io._
import scalaz.\/
import org.scalatest.FunSpec
import org.scalatest.Matchers


class RunParsed extends FunSpec with Matchers {

  val conf: Config = ConfigFactory.load()
  val testsDir = conf.getString("shexSyntaxTestsFolder")
  val parsedSchemasDir = testsDir + "parsedSchemas"
  val schemasDir = testsDir + "schemas"
  val negativeSyntaxDir = testsDir +  "negativeSyntax"
  

  // method borrowed from: Alvin Alexander's Scala cookbook 
  def getFilesFromFolder(path: String): List[(File)] = {
    val d = new File(path)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  
  def file2json(file:File): Try[Json] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8")
      val parsed = JsonParser.parse(contents.mkString)
      parsed.fold(_ => 
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }
  
  case class SchemaAST(
      typ: String,
      prefixes: List[(String,String)],
      shapes: List[ShapeAST]
  )
  
  case class ShapeAST(
      label: String,
      typ: String
  )
  
  def json2AST(json:Json): Try[SchemaAST]  = {
    ???
  }
  
  def parseFolderFiles[A](
      folder: String, 
      parser: File => Try[A]):List[(File,A)] = {
    val files = getFilesFromFolder(folder)
    files.map(file => {
     val t = parser(file)
     if (t.isFailure) {
       println(s"Failure with file ${file.getName}: $t")
     }
     t.map(a => (file,a))
    }).filter(_.isSuccess).map(_.get)
  } 
  
  def getParsedSchemas(jsonSchemasDir: String):List[(File,Json)] = {
    parseFolderFiles(jsonSchemasDir, file2json)
  }
  
  describe("Run JSON tests") {
    val parsedSchemas = getParsedSchemas(parsedSchemasDir)
    for ((file,json) <- parsedSchemas) {
      it(s"Should handle ${file.getName}") {
      }
    }
  }

}