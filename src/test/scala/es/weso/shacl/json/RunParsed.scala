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
import org.scalatest.Matchers
import scalaz._, Scalaz._
import es.weso.shacl.json.AST._
import es.weso.shacl._

class RunParsed extends FunSpec with Matchers {

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
      val contents = io.Source.fromFile(file)("UTF-8")
      val parsed = Parse.decodeValidation[SchemaAST](contents.mkString)
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
  
  def testFile(file: File): Unit = {
    val tryConversion = for {
          schemaAST <- file2AST(file)
          schema <- AST2Schema.cnvAST(schemaAST)
          shaclFile <- lookupFileWithSameName(file,schemasDir,"shex")
          shacl <- parseShaclSchema(shaclFile)
        } yield (schemaAST,schema,shacl)
        tryConversion match {
          case TrySuccess((schemaAST,schema, shacl)) => {
           schemasEqual(schema,shacl) 
          }
          case TryFailure(e)      => fail("Failure: " + e)
        }
  }
  
  describe("Run specific JSON test") {
    val name = "1dotLNex"
    val file = new File(parsedSchemasDir + "/" + name + ".json")
    it (s"should pass file $name") {
     testFile(file) 
    }
  }

  describe("Run JSON tests") {
    val parsedSchemas = getParsedSchemas(parsedSchemasDir)
    for ((file, json) <- parsedSchemas) {
      it(s"Should handle ${file.getName}") {
        testFile(file)
      }
    }
  }

  describe("AST<->JSON") {

    it("should parse simple valueClass with nodekind BNode") {
      val str = """|{ "type": "valueClass",
                   |  "nodeKind": "bnode",
                   |  "length": 20
                   |} """.stripMargin

      val vc = ValueClassAST.empty.copy(
        nodeKind = Some("bnode"),
        length = Some(20),
        maxInclusive = None)
      checkJsonDecoder(str, vc)
    }

    it("should parse empty valueClass") {
      val str = """|{ "type": "valueClass"
                   |} """.stripMargin

      val vc = ValueClassAST.empty
      checkJsonDecoder(str, vc)
    }

    it("should parse single tripleConstraint") {
      val str = """|{ "type": "tripleConstraint",
                   |  "predicate": "http://a.example/p1",
                   |  "value": { "type": "valueClass",
                   |  "nodeKind": "literal",
                   |  "maxinclusive": 5 }
                   |}""".stripMargin

      val vc = ExpressionAST.empty.copy(
        _type = "tripleConstraint",
        predicate = "http://a.example/p1",
        value = ValueClassAST.empty.copy(
          nodeKind = Some("literal"),
          maxInclusive = Some(5)))
      checkJsonDecoder(str, vc)
    }

    it("should parse list of prefixes") {
      val str = """|{ "prefixes" : 
                   |   { "p1": "prefix1",
                   |     "p2": "prefix2" },
                   |  "shapes": {}
                   |}""".stripMargin

      val expected =
        SchemaAST.empty.copy(
          prefixes = Some(Map("p1" -> "prefix1", "p2" -> "prefix2")),
          shapes = Some(Map()))
      checkJsonDecoder(str, expected)
    }

    it("should parse list of shapes") {
      val str = """|{ "prefixes": {},
                   |  "shapes" : 
                   |   { "http://a.example/IssueShape": {
                   |     "type": "shape",
                   |     "expression": {
                   |        "type": "tripleConstraint",
                   |        "predicate": "http://a.example/p1",        
                   |        "value": { "type": "valueClass" }
                   |       }
                   |     }
                   |   }
                   |}""".stripMargin

      val expected =
        SchemaAST.empty.copy(
          shapes = Some(Map("http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = "http://a.example/p1",
                value = ValueClassAST.empty))))),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)
    }

    it("should parse inverse negated shapes") {
      val str = """|{
                   |  "type": "schema",
                   |    "prefixes": {},
                   |      "shapes":{
                   |          "http://a.example/IssueShape": {
                   |                "type": "shape",
                   |                "expression": {
                   |                   "type": "tripleConstraint",
                   |                   "inverse": true,
                   |                   "negated": true,
                   |                   "predicate": "http://a.example/p1",
                   |                   "value": { "type": "valueClass" }}}
                   |}}""".stripMargin
      val expected =
        SchemaAST.empty.copy(
          shapes = Some(Map("http://a.example/IssueShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = "http://a.example/p1",
                value = ValueClassAST.empty,
                negated = Some(true),
                inverse = Some(true)))))),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)
    }
    
   it("should parse references") {
     val str = """{
  "type": "schema",
  "prefixes": {},
  "shapes": {
    "http://a.example/EmployeeShape": {
      "type": "shape",
      "expression": {
        "type": "tripleConstraint",
        "predicate": "http://a.example/p2",
        "value": { "type": "valueClass", "reference": "http://a.example/PersonShape" }
      }
    }
  }
}
"""
 val expected =
     SchemaAST.empty.copy(
       shapes = 
          Some(Map(
            "http://a.example/EmployeeShape" ->
            ShapeAST.empty.copy(
              expression = Some(ExpressionAST.empty.copy(
                _type = "tripleConstraint",
                predicate = "http://a.example/p2",
                value = ValueClassAST.empty.copy(
                    reference=Some(ReferenceAST(Left("http://a.example/PersonShape")))
                  )
                )))                
           )),
          prefixes = Some(Map()))
      checkJsonDecoder(str, expected)     
   }

  }

  def checkJsonDecoder[A: DecodeJson](
    str: String,
    expected: A): Unit = {
    val parsed = str.decodeValidation[A]
    parsed match {
      case Success(v) => v should be(expected)
      case Failure(e) => { fail(e) }
    }
  }

}