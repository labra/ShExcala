package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeDoc._
import es.weso.rdfNode._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.parser.PrefixMap

class ShapeParserSpec 
	extends FunSpec 
	with Matchers 
	with Checkers {

 describe("Shape Parser") {
   
   describe("directive") {
     implicit val s = ShapeParserState.initial
     val str=
       """PREFIX : <http://example.org/>
         |:s {}"""
     info("Parsing prefix")
       
   }
 }   
    
  
}