package es.weso.shex

import util._

case class DataFormat(name: String)

object DataFormat {
  lazy val TURTLE = DataFormat("TURTLE")
  lazy val RDFXML = DataFormat("RDF/XML")
  lazy val NTRIPLES = DataFormat("N-TRIPLES")
  lazy val RDFJSON = DataFormat("RDF/JSON")
  lazy val TRIG = DataFormat("TRIG")
  

  lazy val formats : Seq[DataFormat] = 
    List(TURTLE, RDFXML, NTRIPLES, RDFJSON, TRIG)

  lazy val formatNames : Seq[String] = 
    formats.map(_.name)
    
  def available(format: String): Boolean = {
    formatNames.contains(format.toUpperCase)
  }

  def default = TURTLE

  lazy val toList: List[String] = formats.map(_.name).toList

  override def toString(): String = {
    toList.mkString(",")
  }
  
  def lookup(format: String): Try[DataFormat] = {
    formats.find(_.name == format.toUpperCase).headOption match {
      case Some(df) => Success(df)
      case None => Failure(new Exception(s"Not found format $format in ${formats.toList}"))  
    }
  }
}