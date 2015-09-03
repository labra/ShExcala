package es.weso.shacl

import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import es.weso.monads.Result._
import es.weso.monads._
import util._

class MatchValueSetSpec
    extends FunSpec
    with ShaclValidator
    with Matchers
    with Checkers {

  ignore("matchValueSet") {
    
    it("containsNode should return true if it contains a node") {
      val obj: RDFNode = IRI("b")
      val set = Seq(
          ValueIRI(IRI("a")),
          ValueIRI(IRI("b")),
          ValueIRI(IRI("c"))
          )
      ??? // containsNode(obj,set) should be(true)    
    }
    
    it("containsNode should return false if it doesn't contain a node") {
      val obj: RDFNode = IRI("d")
      val set = Seq(
          ValueIRI(IRI("a")),
          ValueIRI(IRI("b")),
          ValueIRI(IRI("c"))
          )
      ??? // containsNode(obj,set) should be(false)    
    }
    
    it("Should validate a value Set") {
      val obj: RDFNode = IRI("b")
      val vs = ValueSet(Seq(
          ValueIRI(IRI("a")),
          ValueIRI(IRI("b")),
          ValueIRI(IRI("c"))
          ))
      val ctx = Context.emptyContext
      ??? // matchValueSet(obj, vs, ctx).isValid should be(true)
    }
  }
  
}

