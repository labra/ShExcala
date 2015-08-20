package es.weso.shacl

import org.apache.jena.riot.RDFLanguages._
import scala.collection.JavaConversions._
// import es.weso.shacl.DataFormats

object SchemaFormats {

  /**
   * Available list of formats
   */
  lazy val formats = List("SHEXC", "JSONAST") ++ DataFormats.toList

  /**
   * Check if a format is available
   */
  def available(format: String): Boolean = {
    formats.contains(format.toUpperCase)
  }

  lazy val toList: List[String] = formats

  override def toString(): String = {
    toList.mkString(",")
  }
  
  /**
   * Default schema format
   */
  def default = formats(0)
  
}