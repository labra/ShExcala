package es.weso.rdf

import org.scalatest.FunSpec
import com.hp.hpl.jena.rdf.model.ModelFactory
import java.io.ByteArrayInputStream
import com.hp.hpl.jena.rdf.model.Model
import java.io.InputStream
import org.scalatest.Matchers
import es.weso.rdfgraph.nodes._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.jena._

trait RDFBased extends Matchers {

  def shouldBeIsomorphic(r1: RDFReader, r2: RDFReader): Unit = {
    val str1 = r1.serialize("TURTLE")
    val str2 = r2.serialize("TURTLE")
    val m1 = RDFAsJenaModel.fromChars(str1,"TURTLE").get.model
    val m2 = RDFAsJenaModel.fromChars(str2,"TURTLE").get.model
    val b = m1.isIsomorphicWith(m2)
    if (!b) {
      println("Models are not isomorphic")
      println("-------------- Model 1:" + str1)
      println("-------------- Model 2:" + str2)
    }
    b should be(true)
  }

}