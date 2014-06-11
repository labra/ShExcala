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

class ShapeValidatorValueObject 
	extends FunSpec 
	with Matchers 
	with Checkers {
  

 describe("Shape Validator for ValueObject") {
   it("Should validate value object") {
     val obj1: RDFNode = StringLiteral("a")
     val vobj: ValueObject = RDFNodeObject(StringLiteral("a"))
     matchValueObject(obj1)(vobj).run should be(Stream(true))
   }

   it("Should not validate value object if they are different") {
     val obj1: RDFNode = StringLiteral("a")
     val vobj: ValueObject = RDFNodeObject(StringLiteral("b"))
     matchValueObject(obj1)(vobj).run should be(Stream(false))
   }
   
 }
}
 
