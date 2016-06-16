package es.weso.shex

import java.io.File

import scala.util.{Try, Failure => TryFailure, Success => TrySuccess}
import org.scalatest._
import com.typesafe.config._
import es.weso.shex.validation.Validation.Validation
import es.weso.shex.jast._
import es.weso.shex.jast.AST._
import es.weso.utils.testUtils._
import argonaut.{Json, Parse}

class Driver extends FunSpec
    with Matchers with TryValues with TestUtils {

  val conf: Config           = ConfigFactory.load()
  val schemasFolder          = conf.getString("schemasFolder")
  val negativeSyntaxFolder   = conf.getString("negativeSyntaxFolder")
  val validationFolder       = conf.getString("validationFolder")

  def str2AST(str: String): Try[SchemaAST] = {
    parseAST(str)
  }

  def file2AST(file: File): Try[SchemaAST] = {
    val contents = io.Source.fromFile(file)("UTF-8").mkString
    str2AST(contents)
  }

  def getParsedSchemas(jsonSchemasDir: String): List[(File, Json)] = {
    parseFolderFiles(jsonSchemasDir, file2json)
  }

  def parseShExSchema(file: File): Try[Schema] = for {
    (schema, prefixMap) <- Schema.fromString(io.Source.fromFile(file)("UTF-8").mkString)
  } yield schema

  def schemasEqual(fromast: Schema, parsed: Schema): Unit = {
    if (fromast == parsed) {
      info("Schemas are equal")
    } else {
      fail(s"Schemas different\nFrom AST:${fromast.showShapes}\nParsed  :${parsed.showShapes}")
    }
  }


  def str2SchemaAST(str: String): Try[SchemaAST] = {
    trying("Reading AST", str2AST(str))
  }

  def astFile(file: File): Try[SchemaAST] = {
    trying("Reading AST", file2AST(file))
  }

  def astSchema(ast: SchemaAST): Try[Schema] = {
    trying("AST->Schema", AST2Schema.cnvAST(ast))
  }

  def testComparingSchemas(file: File, verbose: Boolean = false): Unit = {
    val tryConversion = for {
      schemaAST <- getASTFromFile(file)
      schema <- {
        if (verbose) info("schemaAST: " + schemaAST.toJsonString) 
        astSchema(schemaAST)
      }
      shaclFile <- {
        if (verbose) info("Schema: " + schema) 
        lookupFileWithSameName(file, schemasFolder, "shex")
      }
      shacl <- {
        val s = trying("Parsing SHACL", parseShExSchema(shaclFile))
        if (verbose) info(s"Parsed: ${s}")
        s
      }
    } yield (schemaAST, schema, shacl)
    tryConversion match {
      case TrySuccess((schemaAST, schema, shacl)) => {
        schemasEqual(schema, shacl)
      }
      case TryFailure(e) => fail("Failure: " + e)
    }
  }

  def testComparingJsons(file: File, verbose: Boolean = false): Unit = {
    val tryConversion = for {
      json <- { jsonFile(file) }
      shaclFile <- { lookupFileWithSameName(file, schemasFolder, "shex") }
      shacl <- trying("Parsing SHACL", parseShExSchema(shaclFile))
      ast <- {
        if (verbose) {
          info("shacl: " + shacl.serialize("SHEXC"))
        }
        Schema2AST.cnvSchema(shacl)
      }
    } yield (json, ast)
    tryConversion match {
      case TrySuccess((json, ast)) => {
        jsonsEqual(json, ast.toJson, verbose)
      }
      case TryFailure(e) => fail("Failure: " + e)
    }
  }
  
  def getValidations(validationsDir: String): List[(File, Json)] = {
    parseFolderFilesWithExt(validationsDir, file2json,"val")
  } 

  def getValidationFromFile(file: File): Try[Validation] = {
    trying("Reading Validation...", file2Validation(file))
  }
  
  def getASTFromFile(file: File): Try[SchemaAST] = {
    trying("Reading Validation...", file2AST(file))
  }
  
  def file2Validation(file: File): Try[Validation] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8").mkString
      val parsed = Parse.decodeValidation[Validation](contents)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }



}