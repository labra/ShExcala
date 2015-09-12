package es.weso.shacl.ast

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
import es.weso.shacl.ast.AST._
import es.weso.shacl._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._

class RunASTSingle extends Driver {
  
  describe("Run specific JSON test") {
    val name = "1val1STRING_LITERAL_LONG2_with_subtag"
    val file = new File(parsedSchemasDir + "/" + name + ".json")
    val contents = io.Source.fromFile(file)("UTF-8").mkString
    val json = Parse.parse(contents).getOrElse(jEmptyObject)
    
    it (s"should pass file $name") {
         val tryConversion = for {
          schemaAST <- {
           println(s"JSON\n${json.spaces2}")
           trying("Reading AST", file2AST(file)) 
          }
          schema <- {
           println(s"SchemaAST\n$schemaAST") 
           trying("AST->Schema", AST2Schema.cnvAST(schemaAST)) 
          }
          shaclFile <- lookupFileWithSameName(file,schemasDir,"shex")
          shacl <- {
           val shacl = trying("Parsing SHACL", parseShaclSchema(shaclFile))
           println(s"SHACL\n$shacl")
           shacl
          }
        } yield (schemaAST,schema,shacl)
        
        val literal1 = ValueLiteral(LangLiteral("pepe",Lang("es")))
        val literal2 = ValueLiteral(LangLiteral("pepe",Lang("es")))
        println("Literals equal? " + (literal1 == literal2))
        
        tryConversion match {
          case TrySuccess((schemaAST,schema, shacl)) => {
           schemasEqual(schema,shacl) 
          }
          case TryFailure(e)      => fail("Failure: " + e)
        }
    }
  }

 
}