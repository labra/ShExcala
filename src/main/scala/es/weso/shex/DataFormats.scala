package es.weso.shex

import org.apache.jena.riot.RDFLanguages._
import scala.collection.JavaConversions._

object DataFormats {

  lazy val formats = List("TURTLE", "RDF/XML", "N-TRIPLES", "RDF/JSON", "TRIG")

  def available(format: String): Boolean = {
    formats.contains(format.toUpperCase)
  }

  def default = "TURTLE"

  lazy val toList: List[String] = formats

  override def toString(): String = {
    toList.mkString(",")
  }
}