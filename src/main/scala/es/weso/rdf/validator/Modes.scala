package es.weso.rdf.validator

import org.apache.jena.riot.RDFLanguages._
import scala.collection.JavaConversions._

object Modes {

  lazy val formats = List("DECLARED", "ALL_NODESLABELS" )

  def available(format: String): Boolean = {
    formats.contains(format.toUpperCase)
  }

  def default = formats(0)

  lazy val toList: List[String] = formats

  override def toString(): String = {
    toList.mkString(",")
  }
  
  def show: String = {
    toString
  }
}