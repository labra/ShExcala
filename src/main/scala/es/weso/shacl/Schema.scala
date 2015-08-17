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

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 *
 */

case class Schema(
    pm: PrefixMap,
    shaclSchema: SHACLSchema) extends Positional {

  /* override def toString(): String = {
    ShaclDoc.schema2String(shaclSchema)(pm)
  } */

  def showShapes: String = {
    shaclSchema.shapes.toString
  }

  def show: String = {
    ShaclDoc.schema2String(shaclSchema)(pm)
  }

  def serialize(format: String): String = {
    format match {
      case "SHEXC" => toString
      case x =>
        if (DataFormats.available(x)) {
          val rdf = RDFAsJenaModel.empty
          Schema2RDF.schema2RDF(this, rdf)
          rdf.serialize(format)
        } else "<<Unknown format: " + format + ">>"
    }
  }

  def labels: Set[Label] = {
    shaclSchema.labels
  }

}

object Schema {

  def empty = Schema(
    pm = PrefixMap.empty,
    shaclSchema = SHACLSchema.empty)

  def fromString(cs: CharSequence, format: String = "SHEXC"): Try[(Schema, PrefixMap)] = {
    format match {
      case "SHEXC" | "SHACLC" =>
        ShaclParser.parse(cs) match {
          case s @ Success(_) => s
          case Failure(t)     => Failure(throw new Exception("Parsing schema: " + t.getMessage))
        }
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
