package es.weso.shex

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.shex.Typing._
import Stream._
import es.weso.rdfgraph.nodes._

class TypingSpec 
 extends FunSpec 
 with Matchers 
 with Checkers {

 describe("Typing") {
   
  it("Should handle empty typing") {
    val t = emptyTyping
    t.hasType(IRI("http://kiko.org")) should be (Set())
  }
 
  it("Should handle a single typing") {
    val t = for ( t0 <- emptyTyping.addType(IRI("x"),IRI("a"))
                ; t1 <- t0.addType(IRI("x"),IRI("b"))
                ) yield t1
    t.get.hasType(IRI("x")) should be (Set(IRI("a"),IRI("b")))
  }

  it("Should handle a single typing with no matches") {
    val t = for ( t0 <- emptyTyping.addType(IRI("x"),IRI("a"))
                ; t1 <- t0.addType(IRI("x"),IRI("b"))
                ) yield t1
    t.get.hasType(IRI("y")) should be (Set())
  }
}
}