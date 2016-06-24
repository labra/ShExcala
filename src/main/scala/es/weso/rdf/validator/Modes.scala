package es.weso.rdf.validator

import org.apache.jena.riot.RDFLanguages._
import scala.collection.JavaConversions._


object Modes {

  case class Mode(name: String, shortName: String, description: String) {
    override def toString = {
      name + " (" + shortName + ")"
    }
  }

  lazy val declared = Mode("Declared", "d", "Validates scope node declarations")
  lazy val allNodes_allLabels = Mode("AllNodes_AllLabels", "a", "Validates all nodes against all labels")
  lazy val allNodes_start = Mode("AllNodes_Start", "s", "Validates all nodes against labels declared in start")

  lazy val modes = List(declared, allNodes_allLabels, allNodes_start)

  lazy val formats = modes.map(_.name).map(_.toUpperCase)
  lazy val shorts = modes.map(_.shortName).map(_.toString.toUpperCase)

  def lookup(format: String): Option[Mode]= {
    val upperFormat = format.toUpperCase
    val filtered = modes.filter(m =>
      m.name.toUpperCase == upperFormat ||
      m.shortName.toUpperCase == upperFormat)
    if (filtered.isEmpty) None
    else Some(filtered.head)
  }

  def default = formats(0)


  override def toString(): String = {
    modes.map(_.toString).mkString(",")
  }
  
  def show: String = {
    toString
  }
}