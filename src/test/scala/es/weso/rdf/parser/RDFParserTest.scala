package es.weso.rdf.parser

import org.scalatest._
import util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdfgraph.nodes._
import es.weso.rdf.parser._

class RDFParserTest extends FunSpec with Matchers with RDFParser with TryValues {

  describe("RDFParser") {
    it("Parses RDF with a simple statement") {
      val cs = """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
      val try1 = for {
        rdf <- RDFAsJenaModel.fromChars(cs,"TURTLE")
        val n : RDFNode = IRI("http://example.org/x")
        val p : IRI = IRI("http://example.org/p")
        obj <- iriFromPredicate(p)(n,rdf)
      } yield (obj)
      try1.success.value should be (IRI("http://example.org/T"))
    }
  }
}
