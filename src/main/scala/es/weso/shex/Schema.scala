package es.weso.shex

import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.rdf._
import es.weso.shex.parser._
import es.weso.shex.converter._
import es.weso.shex.PREFIXES._
import scala.util.parsing.input.Positional
import scala.util.{ Try, Success, Failure }
import es.weso.monads._
import org.slf4j._
import es.weso.utils.IO._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.jast.AST._
import es.weso.shex.jast._
import argonaut._
import Argonaut._
import es.weso.rbe._
import java.net.URI
import io._
import org.slf4s._

/**
 * A Schema
 * 
 * @param pm prefix map
 * @param shexSchema ShExSchema
 */
case class Schema(
    pm: PrefixMap,
    shexSchema: ShExSchema) 
      extends Positional with Logging {

 def matchNodesLabels( 
      decls: Seq[(RDFNode,Label)], 
      rdf: RDFReader) = {
    val seSchema = SEShEx.shex2SE(shexSchema)
    seSchema.matchNodesLabels(decls, RDFAsGraph(rdf))
  } 
 
  def matchNode_Label(node: RDFNode, label: Label, rdf: RDFReader) = {
    log.info(s"Trying to match $node with $label") 
    val seSchema = SEShEx.shex2SE(shexSchema)
    log.info(s"SESchema: $seSchema") 
    seSchema.matchNode(node, label, RDFAsGraph(rdf))
  }

  def showShapes: String = {
    shexSchema.shapes.toString
  }

  def show: String = {
    ShExDoc.schema2String(shexSchema)(pm)
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
        if (DataFormat.available(x)) {
          val rdf = RDFAsJenaModel.empty
          Schema2RDF.schema2RDF(this, rdf)
          rdf.serialize(format)
        } else "<<Unknown format: " + format + ">>"
    }
  }

  def labels: List[Label] = {
    shexSchema.labels
  }

}

object Schema extends Logging {

  def empty = Schema(
    pm = PrefixMap.empty,
    shexSchema = ShExSchema.empty)
    
    def str2AST(str: String): Try[SchemaAST] = {
    Try{
      val parsed = Parse.decodeValidation[SchemaAST](str)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing : $str"),
        x => x)
    }
  }
 

  def fromString(cs: CharSequence, format: String = "SHEXC", base: Option[String] = None): Try[(Schema, PrefixMap)] = {
    format.toUpperCase match {
      case "SHEXC" | "SHACLC" => {
        val baseIRI : IRI = IRI(base.getOrElse(""))
        ShExParser.parse(cs,baseIRI) match {
          case s @ Success(_) => s
          case Failure(t)     => Failure(throw new Exception("Parsing schema: " + t.getMessage))
        }
      }
      case ("JSONAST" | "JSON") => // TODO: Handle base URI for JSON format? 
       for {
        schemaAST <- str2AST(cs.toString)
        schema <- AST2Schema.cnvAST(schemaAST)
      } yield (schema,schema.pm)
      case x =>
        if (SchemaFormat.available(x)) {
          for {
            rdf <- RDFAsJenaModel.fromChars(cs, format, base)
            (schema, pm) <- RDF2Schema.rdf2Schema(rdf)
          } yield (schema, pm)
        } else
          Failure(throw new Exception("Unsupported format: " + x))
    }
  }

  def fromFile(fileName: String, format: String, base: Option[String] = None): Try[(Schema, PrefixMap)] = {
    for (
      cs <- getContents(fileName); (schema, prefixMap) <- Schema.fromString(cs, format, base)
    ) yield (schema, prefixMap)
  }

  def fromURI(uri: URI, format: String, base: Option[String] = None): Try[(Schema, PrefixMap)] = {
    log.info(s"Reading contents behind URI: $uri")
    val cs = Source.fromURI(uri)(Codec.UTF8).mkString
    log.info(s"Contents: $cs")
     for {
      (schema, prefixMap) <- Schema.fromString(cs, format, base)
    } yield (schema, prefixMap)
  }

}
