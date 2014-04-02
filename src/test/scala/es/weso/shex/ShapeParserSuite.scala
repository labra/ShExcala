package es.weso.shex

import org.scalatest._
import org.scalatest.prop._

import es.weso.rdfNode._
import es.weso.shex.ShapeDoc._
import es.weso.shex.ShapeSyntax._
import scala.Either._

class ShapeParserSuite extends FunSpec 
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

   it("should parse prefixes") {
     val state = ShapeParserState.initial
     val str = "prefix a: <http://example.org/a/> " 
     val result = ShapeParser.parse(ShapeParser.directive(state),str)
     val expected = state.addPrefix("a",IRI("http://example.org/a/"))
     result.get should be (expected)
   }
   
       
   ignore("should parse statement with prefixes") {
     val state = ShapeParserState.initial
     val str = "prefix a: <http://example.org/a/> \n" +
               "prefix b: <http://example.org/b/> \n" 
     
     val result = ShapeParser.parse(ShapeParser.shExParser(state),str)
     val expected = state.
     				addPrefix("a",IRI("http://example.org/a/")).
     				addPrefix("b",IRI("http://example.org/b/"))
     val noShapes : List[List[Shape]] = List(List())
     result.get._1 should be (noShapes)
     result.get._2 should be (expected)
      
   }  

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
   
   describe("Shapes") {

     it("Should parse labels") {
       val state = ShapeParserState.initial
       val iri = "Label"
       val str = "<" + iri + ">"
       val result = ShapeParser.parse(ShapeParser.label(state),str)
       result.get should be (IRILabel(IRI(iri)))
     }
     
     it("Should parse qualified labels ") {
       val prefix = "http://example.org/"
       val localName = "a"
       val alias = "ex"
       val str = alias + ":" + localName 
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = ShapeParser.parse(ShapeParser.label(state),str)
       result.get should be (IRILabel(IRI(prefix + localName)))
     }

     it("Should parse openParen") {
       val str = "("
       val result = ShapeParser.parse(ShapeParser.openParen,str)
       result.get should be ("(")
     }

     it("Should parse fixedValues with qualified value type") {
       val prefix = "http://example.org/"
       val localName = "a"
       val alias = "ex"
       val str = alias + ":" + localName  
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = ShapeParser.parse(ShapeParser.fixedValues(state),str)
       result.get._1 should be (ValueType(IRI(prefix + localName)))
     }

     it("Should parse fixedValues with a set of one qualified value") {
       val prefix = "http://example.org/"
       val a = "a"
       val alias = "ex"
       val str = "( " + alias + ":" + a + " )"  
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = ShapeParser.parse(ShapeParser.fixedValues(state),str)
       result.get._1 should be (ValueSet(Seq(IRI(prefix + a))))
     }

     it("Should parse fixedValues with a set of two qualified values ") {
       val prefix = "http://example.org/"
       val a = "a"
       val b = "b"
       val alias = "ex"
       val str = "( " + alias + ":" + a + " " + alias + ":" + b + " )"  
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = ShapeParser.parse(ShapeParser.fixedValues(state),str)
       result.get._1 should be (ValueSet(Seq(IRI(prefix + a), IRI(prefix + b))))
     }

     it("Should parse fixedValues with a reference ") {
       val label = "http://example.org/a"
       val str = "@<" + label + ">"   
       val state = ShapeParserState.initial
       val result = ShapeParser.parse(ShapeParser.fixedValues(state),str)
       result.get._1 should be (ValueReference(IRILabel(IRI(label))))
     }

     it("Should parse nameClassAndValue - single iri ") {
       val prefix = "http://example.org/"
       val a = "a"
       val b = "b"
       val alias = "ex"
       val str = alias + ":" + a + " " + alias + ":" + b    
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = ShapeParser.parse(ShapeParser.nameClassAndValue(state),str)
       result.get._1._1 should be (NameTerm(IRI(prefix + a)))
       result.get._1._2 should be (ValueType(IRI(prefix + b)))
     }
   }
 }   
    
  
}