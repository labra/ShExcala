package es.weso.shex

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.shex.Typing._
import Stream._
import es.weso.rdfgraph.nodes._
import es.weso.rdf.RDFTriples
import es.weso.rdf.RDF
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.parser.PrefixMap
import es.weso.shex.ShapeSyntax._

class ContextSpec 
 extends FunSpec 
 with Matchers 
 with Checkers {

 describe("Context") {
   
  it("Should return IRIs of two triples") {
    val rdf : RDF = 
       RDFTriples(triples = Set( RDFTriple(IRI("a"),IRI("p"),IRI("x"))
                               , RDFTriple(IRI("a"),IRI("q"),IRI("x"))
                               )
                 , pm = PrefixMap.empty
                 )
           
     val shEx = ShEx(rules=Seq(),start=None)
     val ctx = Context(rdf=rdf,shEx=shEx, typing = Typing.emptyTyping)
     ctx.getIRIs.toSet should be(Set(IRI("a"),IRI("p"),IRI("q"),IRI("x")))
  }

  it("Should return IRIs of a single triple") {
     val epm = PrefixMap.empty
     val g = RDFTriples(triples= Set(RDFTriple(IRI("a"),IRI("p"),StringLiteral("hi"))), pm=epm)
     val shape = Shape(label = IRILabel(IRI("l")), 
         			   rule = ArcRule(id = None, n = NameTerm(IRI("p")), v = typeXsdString )
         			  )
     val ctx = Context(rdf=g,
         shEx = ShEx(rules=Seq(shape),start =None),
         typing = Typing.emptyTyping)
     ctx.getIRIs.toSet should be(Set(IRI("a"),IRI("p")))
  }

 }
}