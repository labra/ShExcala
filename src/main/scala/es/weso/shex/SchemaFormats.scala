package es.weso.shex

import org.apache.jena.riot.RDFLanguages._
import scala.collection.JavaConversions._

object SchemaFormats {

  lazy val formats = List("SHEXC") ++ DataFormats.toList

  def available(format: String): Boolean = {
    formats.contains(format.toUpperCase)
  }

  lazy val toList: List[String] = formats

  override def toString(): String = {
    toList.mkString(",")
  }
}