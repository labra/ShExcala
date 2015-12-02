package es.weso.shacl

import org.apache.jena.riot.RDFLanguages._
import scala.collection.JavaConversions._
// import es.weso.shacl.DataFormats

case class SchemaFormat(name: String) 

object SchemaFormats {
  
  lazy val shexc = SchemaFormat("SHEXC")
  lazy val jsonast = SchemaFormat("JSONAST")
  lazy val shexFormats : Seq[SchemaFormat] = Seq(shexc,jsonast)
  lazy val rdfFormats : Seq[SchemaFormat] = DataFormats.toList.map(SchemaFormat(_))

  /**
   * Available list of formats
   */
  lazy val formats : Seq[String] = shexFormats.map(_.name) ++ DataFormats.toList

  /**
   * Check if a format is available
   */
  def available(format: String): Boolean = {
    formats.contains(format.toUpperCase)
  }

  lazy val toList: List[String] = formats.toList

  override def toString(): String = {
    toList.mkString(",")
  }
  
  /**
   * Default schema format
   */
  def default = formats(0)
  
}