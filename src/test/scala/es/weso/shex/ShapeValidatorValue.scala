package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.shex.ShapeValidator._
import es.weso.shex.Typing._
import es.weso.shex.Context._
import es.weso.parser.PrefixMap
import es.weso.rdf.RDFTriples

class ShapeValidatorValue
	extends FunSpec 
	with Matchers 
	with Checkers {
  

 describe("Shape Validator for Value") {
   it("Should validate value set") {
     val ctx = Context.emptyContext
     val obj1: RDFNode = StringLiteral("a")
     val vset: ValueClass = ValueSet(Seq(RDFNodeObject(StringLiteral("a"))))
     matchValue(ctx,obj1,vset).isValid should be(true)
   }

   it("Should not validate value set if different") {
     val ctx = Context.emptyContext
     val obj1: RDFNode = StringLiteral("a")
     val vset: ValueClass = ValueSet(Seq(RDFNodeObject(StringLiteral("b"))))
     matchValue(ctx,obj1,vset).isValid should be(false)
   }

   it("Should validate value set if more than one match") {
     val ctx = Context.emptyContext
     val obj1: RDFNode = StringLiteral("a")
     val vset: ValueClass = ValueSet(Seq(RDFNodeObject(StringLiteral("a")),RDFNodeObject(StringLiteral("a"))))
     matchValue(ctx,obj1,vset).isValid should be(true)
   }

   it("Should validate value string") {
     val ctx = Context.emptyContext
     val obj1: RDFNode = StringLiteral("a")
     val v: ValueClass = typeXsdString
     matchValue(ctx,obj1,v).isValid should be(true)
   }

     it("Should not validate value string") {
     val ctx = Context.emptyContext
     val obj1: RDFNode = IntegerLiteral(23)
     val v: ValueClass = typeXsdString
     matchValue(ctx,obj1,v).isValid should be(false)
   }
}
}
 
