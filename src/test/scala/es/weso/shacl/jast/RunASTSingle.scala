package es.weso.shacl.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }

import scala.io._

import org.scalatest.FunSpec
import org.scalatest._

import es.weso.shacl.jast.AST._
import es.weso.shacl._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._

class RunASTSingle extends Driver {
  
  describe("Run specific JSON test") {
    val name = "1val1vsMinusiri3"
    val file = new File(parsedSchemasDir + "/" + name + ".json")
    val contents = io.Source.fromFile(file)("UTF-8").mkString
    val json = Parse.parse(contents).getOrElse(jEmptyObject)
    
    it (s"should pass file $name") {
         val tryConversion = for {
          schemaAST <- {
           println(s"Direct JSON\n${json.spaces2}")
           val ast = trying("Reading AST", file2AST(file))
           println(s"Parsed as AST\n${ast.get}")
           ast
          }
          schema <- {
           println(s"schema\n$schemaAST") 
           val s = trying("AST->Schema", AST2Schema.cnvAST(schemaAST))
           println(s"schemaAST to schema:\n$s")
           s
          }
          shaclFile <- lookupFileWithSameName(file,schemasDir,"shex")
          shacl <- {
           val shacl = trying("Parsing SHACL", parseShaclSchema(shaclFile))
           println(s"shacl from parsing shex file\n${shacl}")
           shacl
          }
        } yield (schemaAST,schema,shacl,json)
        
        tryConversion match {
          case TrySuccess((schemaAST,schema, shacl,json)) => {
           println(s"\nJson generated = \n${schemaAST.asJson.spaces2}")
           jsonsEqual(json,schemaAST.asJson)
           
           schemasEqual(schema,shacl) 
          }
          case TryFailure(e)      => fail("Failure: " + e)
        }
    }
  }

 
}