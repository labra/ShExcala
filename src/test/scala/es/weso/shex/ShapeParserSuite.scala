package es.weso.shex

import org.scalatest._
import org.scalatest.prop._

import es.weso.rdfNode._
import es.weso.shex.ShapeDoc._
import es.weso.shex.ShapeSyntax._
import scala.Either._

class ShapeParserSuite
	extends FunSpec 
	with Matchers 
	with Checkers {

 describe("Shape Parser") {

   describe("valueObject") {

	   it ("Should parse a value Object made by <>") {
       val str = "<>"
       
       val state : ShapeParserState = ShapeParserState.initial
       val result = ShapeParser.parse(ShapeParser.valueObject(state),str)
       result.get._1 should be(IRI(""))
       result.get._2 should be(state)
     }  

	it ("Should parse a value Object made by an IRI") {
	   val iri = "http://www.example.org/"
       val str = "<" + iri + ">"
       
       val state : ShapeParserState = ShapeParserState.initial
       val result = ShapeParser.parse(ShapeParser.valueObject(state),str)
       result.get._1 should be(IRI(iri))
       result.get._2 should be(state)
     }  

     it ("Should parse a value Object made by blank node") {
       val str = "_:a"
       val state : ShapeParserState = ShapeParserState.initial
       val (id1,state1) = state.newBNode("a")
       val result = ShapeParser.parse(ShapeParser.valueObject(state),str)
       result.get._1 should be (id1)
       result.get._2 should be (state1)
     }  

     it ("Should parse a value Object made by a qualified name") {
       val prefix = "ex"
       val localname = "a"
       val example = "http://example.org/"
       val strTest = prefix + ":" + localname
       val state : ShapeParserState = ShapeParserState.initial
       val state1 = state.addPrefix(prefix, IRI(example))

       val result = ShapeParser.parse(ShapeParser.valueObject(state1),strTest)
       result.get._1 should be (IRI(example + localname))
       result.get._2 should be (state1)
     }  
   

     it ("Should parse a value Object made by a string") {
       val str = "Hi"
       val strTest = "\"" + str + "\""
       val state : ShapeParserState = ShapeParserState.initial

       val result = ShapeParser.parse(ShapeParser.valueObject(state),strTest)
       result.get._1 should be (StringLiteral(str))
       result.get._2 should be (state)
     }  
   }

 describe("Prefix directives") {

     ignore("Should be able to parse default prefix") {
      val ex = "http://example.org/"
      val str = "PREFIX : <" + ex + ">\n" + 
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
                ":s { :a xsd:string }"

      val result = ShapeParser.parse(str)
      
      val shape : ArcRule = ArcRule(
    		  id = None, 
          	  n = NameTerm(IRI(ex + "a")),
          	  v = ValueType(IRI(xsd + "string")),
          	  c = Default,
          	  a = NoActions)
          	  
      val expected : ShEx = ShEx(
           rules = Seq(Shape(label=IRILabel(IRI("http://example.org/s")), 
        		   	         rule= shape)), 
           start = None)
        
       // result should be (Left(expected))
     }
       
   }
   
   
 }   
    
  
}