package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeParser._
import es.weso.shex.converter._
import es.weso.shex.converter._
import es.weso.shacl.PREFIXES._
import scala.util.parsing.input.Positional
import scala.util.{ Try, Success, Failure }
import es.weso.monads._
import org.slf4j._
import es.weso.utils.IO._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.DataFormats
import es.weso.shacl.SchemaFormats


trait SchemaImpl

case class Schema(
    pm: PrefixMap,
    shEx: ShEx) extends Positional {

  override def toString(): String = {
    ShapeDoc.schema2String(this)(pm)
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

  def getLabels(): List[Label] = {
    if (shEx.start == None) shEx.rules.map(_.label).toList
    else List(shEx.start.get)
  }

  def addAny: Schema =
    this.copy(pm = pm.addPrefix("shex", sh_IRI),
      shEx = shEx.copy(rules = shEx.rules :+ ShapeSyntax.anyShape)
    )

}

object Schema {

  def fromString(cs: CharSequence, format: String = "SHEXC"): Try[(Schema, PrefixMap)] = {
    format match {
      case "SHEXC" =>
        ShapeParser.parse(cs) match {
          case s @ Success(_) => s
          case Failure(t) => Failure(throw new Exception("Parsing schema: " + t.getMessage))
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
