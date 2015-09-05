package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import es.weso.shacl.Shacl._
import es.weso.shacl.parser._
import es.weso.shacl.converter._
import es.weso.shacl.PREFIXES._
import scala.util.parsing.input.Positional
import scala.util.{ Try, Success, Failure }
import es.weso.monads._
import org.slf4j._
import es.weso.utils.IO._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.json.AST._
import es.weso.shacl.json._
import argonaut._
import Argonaut._
import es.weso.rbe._

case class Schema(
    pm: PrefixMap,
    shaclSchema: SHACLSchema) extends Positional {

  def matchNode_Label(node: RDFNode, label: Label, rdf: RDFReader) = {
    val seSchema = SEShacl.shacl2SE(shaclSchema)
    seSchema.matchNode(node, label, RDFAsGraph(rdf))
  }

  def showShapes: String = {
    shaclSchema.shapes.toString
  }

  def show: String = {
    ShaclDoc.schema2String(shaclSchema)(pm)
  }

  def serialize(format: String): String = {
    format.toUpperCase match {
      case "SHEXC" => show
      case "JSONAST" => {
       Schema2AST.cnvSchema(this) match {
         case Success(ast) => {
           ast.asJson.spaces2
         } 
         case Failure(e) => throw new Exception(s"Cannot convert schema $this to format $format")
       }
      }
      case x =>
        if (DataFormats.available(x)) {
          val rdf = RDFAsJenaModel.empty
          Schema2RDF.schema2RDF(this, rdf)
          rdf.serialize(format)
        } else "<<Unknown format: " + format + ">>"
    }
  }

  def labels: List[Label] = {
    shaclSchema.labels
  }

}

object Schema {

  def empty = Schema(
    pm = PrefixMap.empty,
    shaclSchema = SHACLSchema.empty)
    
    def str2AST(str: String): Try[SchemaAST] = {
    Try{
      val parsed = Parse.decodeValidation[SchemaAST](str)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing : $str"),
        x => x)
    }
  }
 

  def fromString(cs: CharSequence, format: String = "SHEXC"): Try[(Schema, PrefixMap)] = {
    format.toUpperCase match {
      case "SHEXC" | "SHACLC" =>
        ShaclParser.parse(cs) match {
          case s @ Success(_) => s
          case Failure(t)     => Failure(throw new Exception("Parsing schema: " + t.getMessage))
        }
        
      case ("JSONAST" | "JSON") => for {
        schemaAST <- str2AST(cs.toString)
        schema <- AST2Schema.cnvAST(schemaAST)
      } yield (schema,schema.pm)
      case x =>
        if (SchemaFormats.available(x)) {
          for {
            rdf <- RDFAsJenaModel.fromChars(cs, format)
            (schema, pm) <- RDF2Schema.rdf2Schema(rdf)
          } yield (schema, pm)
        } else
          Failure(throw new Exception("Unsupported format: " + x))
    }
  }

  def fromFile(fileName: String, format: String): Try[(Schema, PrefixMap)] = {
    for (
      cs <- getContents(fileName); (schema, prefixMap) <- Schema.fromString(cs, format)
    ) yield (schema, prefixMap)
  }

}
