package es.weso.shex

import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.query._
import java.io.StringWriter

object Sparql {
  def query(turtleFile: String, queryFile: String): String = {
    try {
      println("Parsing " + turtleFile)
      val model = ModelFactory.createDefaultModel()
      model.read(turtleFile)
      println("Parsing " + queryFile)
      val query = QueryFactory.read(queryFile)
      val exec = QueryExecutionFactory.create(query, model)
      val result = exec.execConstruct()
      val strWriter = new StringWriter
      model.write(strWriter, "Turtle")
      return strWriter.toString
    } catch {
      case e: Exception => "Exception: " + e
    }

  }
}