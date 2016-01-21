package es.weso.shacl

import org.scalatest._
import es.weso.rdfgraph.nodes._
import es.weso.utils.Checker


class CheckerTest 
 extends FunSpec 
 with Matchers 
 with TryValues {
 describe("Checker") {
   it("Should check a plain IRI") {
     val cond : IRIKind = IRIKind(None,List())
     val node : RDFNode = IRI("http://example.org/x")
     checkOK(cond.check(node)) 
   }
   
   it("Should check a plain IRI with a regex") {
     val cond = IRIKind(None,List(Pattern("x4$")))
     val node : RDFNode = IRI("http://example.org/x4")
     checkOK(cond.check(node)) 
   }
   
   def checkOK[A,E](chk: Checker[A,E]): Unit = {
     if (!chk.isOK) {
       fail(s"Value is not ok: $chk") 
     } 
   }

 }
 
}
