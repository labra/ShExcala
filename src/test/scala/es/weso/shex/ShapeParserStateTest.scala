package es.weso.shex

import org.scalatest._
import org.scalatest.prop._

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.shex.ShapeDoc._
import es.weso.shex.ShapeSyntax._
import scala.Either._

class ShapeParserStateTest
	extends FunSpec 
	with Matchers 
	with Checkers {

 describe("Compare states") {
   
   it ("should compare initial states") {
    val initial1 = ShapeParserState.initial
    val initial2 = ShapeParserState.initial
    initial1 should be(initial2)
   }
   
   it ("should compare states with 2 blank nodes") {
    val initial1 = ShapeParserState.initial
    val (id1,state1) = initial1.newBNode("a")
    val initial2 = ShapeParserState.initial
    val (id2,state2) = initial2.newBNode("a")
    id1 should be(id2)
    state1 should be(state2)
   }
 }
  
}