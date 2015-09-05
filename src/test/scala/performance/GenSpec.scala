package es.weso.performance

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.rdfgraph.nodes._
import es.weso.shex.Schema
import es.weso.shex.Typing
import es.weso.rdf.RDFTriples
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.shacl.PrefixMaps
import es.weso.shex.ShapeDoc
import es.weso.shex.ShExMatcher

class GenSpec
    extends FunSpec
    with Matchers
    with Checkers {

  describe("Generator of Schemas") {

    it("Should generate a schema with 2 rules") {
      val gen2 = GenShape.genAnds(2)
      val g = GenShape.genTriples(2)
      val t: Typing = Typing.emptyTyping.addType(IRI("x"), IRI("a")).get
      info("Shape: " + ShapeDoc.schema2String(gen2)(PrefixMaps.commonShacl))
      info("Triples: " + g.serialize())
      val matcher = ShExMatcher(gen2, g, false, false)
      val result = matcher.matchAllIRIs_AllLabels()
      result.isValid should be(true)
    }

    it("Should generate a schema with 20 rules") {
      val gen = GenShape.genAnds(20)
      val g = GenShape.genTriples(20)
      val t: Typing = Typing.emptyTyping.addType(IRI("x"), IRI("a")).get
      val matcher = ShExMatcher(gen, g, false, false)
      val result = matcher.matchAllIRIs_AllLabels()
      result.isValid should be(true)
    }

    it("Should generate a schema with 200 rules") {
      val gen = GenShape.genAnds(200)
      val g = GenShape.genTriples(200)
      val t: Typing = Typing.emptyTyping.addType(IRI("x"), IRI("a")).get
      val matcher = ShExMatcher(gen, g, false, false)
      val result = matcher.matchAllIRIs_AllLabels()
      result.isValid should be(true)
    }

  }

}