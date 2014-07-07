package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.shex.Typing._
import es.weso.shex.Context._
import es.weso.parser.PrefixMap
import es.weso.rdf.RDFTriples

class ShapeValidatorSpec 
	extends FunSpec
	with ShapeValidatorWithDeriv
	with Matchers 
	with Checkers {
  

 describe("Shape Validator with Types") {
   it("Should validate type IRI") {
     val obj: RDFNode = IRI("a")
     val vtype : RDFNode = shex_IRI
     matchType(obj,vtype).run should be(Stream(true))
   }
   
   it("Should validate type xsd_string") {
     val obj: RDFNode = StringLiteral("a")
     val vtype = xsd_string 
     matchType(obj,vtype).run should be(Stream(true))
   }
   
   it("Should validate type xsd_integer") {
     val obj: RDFNode = IntegerLiteral(23)
     val vtype = xsd_integer 
     matchType(obj,vtype).run should be(Stream(true))
   }

   it("Should validate type xsd_double") {
     val obj = DoubleLiteral(23.5)
     val vtype = xsd_double 
     matchType(obj,vtype).run should be(Stream(true))
   }

   it("Should validate type xsd_double by datatype") {
     val obj: RDFNode = DatatypeLiteral("23.5",xsd_double)
     val vtype = xsd_double 
     matchType(obj,vtype).run should be(Stream(true))
   }

   it("Should not validate type xsd_double with xsd_integer by datatype") {
     val obj: RDFNode = DatatypeLiteral("23.5",xsd_double)
     val vtype = xsd_integer 
     matchType(obj,vtype).run should be(Stream(false))
   }

 }
 
 describe("Shape Validator with Rules") {
   it("Should validate empty rule") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = Set()
     val r = EmptyRule
     matchRule(ctx,g,r).run should be(Stream(emptyTyping))
   }
  
   it("Should not validate rule with an arc with empty triples") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = Set()
     val r = ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
     matchRule(ctx,g,r).isValid should be(false)
   }
   
   it("Should validate rule with a single arc ") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = Set(RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi")))
     val r = ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
     matchRule(ctx,g,r).isValid should be(true)
   }

   it("Should not validate rule with an arc and 2 triples") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi"))
           , RDFTriple(IRI("x"),IRI("b"),StringLiteral("hi"))
           )
     val r = ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
     matchRule(ctx,g,r).isValid should be(false) 
   }

   it("Should validate rule with a value set") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi"))
           )
     val r = ArcRule(id = None, n = NameTerm(IRI("a")), v = ValueSet(Seq(RDFNodeObject(StringLiteral("hi")))))
     matchRule(ctx,g,r).isValid should be(true) 
   }

   it("Should not validate rule with a value set and different value") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi"))
           )
     val r = ArcRule(id = None, n = NameTerm(IRI("a")), v = ValueSet(Seq(RDFNodeObject(StringLiteral("bye")))))
     matchRule(ctx,g,r).isValid should be(false) 
   }

   it("Should validate rule with two arcs and 2 triples") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi"))
           , RDFTriple(IRI("x"),IRI("b"),StringLiteral("hi"))
           )
     val r = AndRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString),
              ArcRule(id = None, n = NameTerm(IRI("b")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }
 
   it("Should validate or rule (left) with two arcs and 1 triple") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi"))
           )
     val r = OrRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString),
              ArcRule(id = None, n = NameTerm(IRI("b")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }
    
   it("Should validate or rule (right) with two arcs and 1 triple") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("b"),StringLiteral("hi"))
           )
     val r = OrRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString),
              ArcRule(id = None, n = NameTerm(IRI("b")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }

   it("Should validate or rule (none) with two arcs and 1 triple") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("c"),StringLiteral("hi"))
           )
     val r = OrRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString),
              ArcRule(id = None, n = NameTerm(IRI("b")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(false) 
   }

    it("Should validate one or more rule with one arc") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi"))
           )
     val r = PlusRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }

    it("Should validate one or more rule with two arcs") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi1"))
           , RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi2"))
           )
     val r = PlusRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }
 
    it("Should validate one or more rule with three arcs") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi1"))
           , RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi2"))
           , RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi3"))
           )
     val r = PlusRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     info("Result: " + matchRule(ctx,g,r))
     matchRule(ctx,g,r).isValid should be(true) 
   }

   it("Should not validate one or more rule with one bad arc") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("b"),StringLiteral("hi1"))
           )
     val r = PlusRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(false) 
   }

    it("Should not validate one or more rule with one bad and one good") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi1"))
           , RDFTriple(IRI("x"),IRI("b"),StringLiteral("hi1"))
           )
     val r = PlusRule(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(false) 
   }

  it("Should validate star rule with three arcs") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set( RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi1"))
           , RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi2"))
           , RDFTriple(IRI("x"),IRI("a"),StringLiteral("hi3"))
           )
     val r = star(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }

  it("Should validate star rule with no arcs") {
     val ctx = emptyContext
     val g : Set[RDFTriple] = 
        Set()
     val r = star(
              ArcRule(id = None, n = NameTerm(IRI("a")), v = typeXsdString)
             )
     matchRule(ctx,g,r).isValid should be(true) 
   }

 }

 describe("ShapeValidator of shapes") {
   
   it("should validate empty shape") {
     val ctx = emptyContext
     val shape = Shape(label = IRILabel(IRI("a")), rule = EmptyRule)
     matchShape(ctx,IRI("a"),shape)
   }

   it("should not validate shape with a triple") {
     val epm = PrefixMap.empty
     val g = RDFTriples(triples= Set(RDFTriple(IRI("a"),IRI("p"),StringLiteral("hi"))), pm=epm)
     val shape = Shape(label = IRILabel(IRI("a")), rule = EmptyRule)
     val ctx = Context(
         rdf=g,
         shEx = ShEx(rules=Seq(shape), start = None), 
         typing = Typing.emptyTyping,
         pm = epm)
     matchShape(ctx,IRI("a"),shape).isFailure should be(true)
   } 
   
   it("should validate shape with a triple") {
     val epm = PrefixMap.empty
     val g = RDFTriples(triples= Set(RDFTriple(IRI("a"),IRI("p"),StringLiteral("hi"))), pm=epm)
     val shape = Shape(label = IRILabel(IRI("l")), 
         			   rule = ArcRule(id = None, n = NameTerm(IRI("p")), v = typeXsdString)
         			  )
     val ctx = Context(rdf=g, 
         shEx = ShEx(rules=Seq(shape),start =None),
         typing = Typing.emptyTyping,
         pm = epm)
     val result = matchShape(ctx,IRI("a"),shape)
     info("Result:\n" + result.toString)
     result.isValid should be(true)
   }
 
 }

 
 describe("ShapeValidator of schema with a single triple - rule") {
   it("should validate empty rule") {
     val epm = PrefixMap.empty
     val g = RDFTriples(triples= Set(RDFTriple(IRI("a"),IRI("p"),StringLiteral("hi"))), pm=epm)
     val shape = Shape(label = IRILabel(IRI("l")), 
         			   rule = ArcRule(id = None, n = NameTerm(IRI("p")), v = typeXsdString)
         			  )
     val shEx= ShEx(rules=Seq(shape),start =None)
     val ctx = Context(rdf=g, 
         shEx = shEx, 
         typing = Typing.emptyTyping,
         pm = epm)
     val schema = Schema(pm = epm, shEx = shEx)
     val result = Schema.matchSchema(IRI("a"), g, schema)
     info("Result:\n" + result.toString)
     result.isValid should be(true)
  }
 }

 describe("ShapeValidator of schema with a single triple and a rule with 2 shapes repeated ") {
   it("should validate empty rule") {
     val epm = PrefixMap.empty
     val g = RDFTriples(triples= Set(RDFTriple(IRI("a"),IRI("p"),StringLiteral("hi"))), pm=epm)
     val shape1 = Shape(label = IRILabel(IRI("label1")), 
         			   rule = ArcRule(id = None, n = NameTerm(IRI("p")), v = typeXsdString)
         			  )
     val shape2 = Shape(label = IRILabel(IRI("label2")), 
         			   rule = ArcRule(id = None, n = NameAny(excl = Set()), v = typeXsdString)
         			  )         			  
     val shEx= ShEx(rules=Seq(shape1,shape2),start =None)
     val ctx = Context(rdf=g, 
         shEx = shEx, 
         typing = Typing.emptyTyping,
         pm = epm
         )
     val schema = Schema(pm = epm, shEx = shEx)
     val result = Schema.matchSchema(IRI("a"), g, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
  }
 }

 describe("ShapeValidator with parser") {

   it("should not validate empty rule") {
     val ctx = emptyContext
     val strShape = "<a> { }"
     val strRDF = "<x> <p> <y> ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(false)
   }
   
    it("should validate single rule") {
     val ctx = emptyContext
     val strShape = "<a> { <p> . }"
     val strRDF = "<x> <p> <y> ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }
   
  it("should validate optOr2") {
     val ctx = emptyContext
     val strShape = "<a> { <p> . ? , <q> . }"
     val strRDF = "<x> <p> 1 ; <q> 2 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }
 }
 
  it("should validate iri") {
     val ctx = emptyContext
     val strShape = "<a> { <p> <http://www.w3.org/2013/ShEx/ns#IRI> }"
     val strRDF = "<x> <p> <i> ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }


  it("should validate with a negation") {
     val ctx = emptyContext
     val strShape = "<a> { ! <p> . }"
     val strRDF = "<x> <q> 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should not validate with a negation that contains a triple") {
     val ctx = emptyContext
     val strShape = "<a> { ! <p> . }"
     val strRDF = "<x> <p> 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(false)
   }

  it("should validate rev of a triple") {
     val strShape = "<a> { ^ <p> . }"
     val strRDF = "<y> <p> <x> ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema, true)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate with a excluded stem ") {
     val strShape = "<a> { - <http://example.org/>~  . }"
     val strRDF = "<x> <http://ex.org/p> 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate with a excluded qualified stem ") {
     val strShape = "prefix e: <http://example.org/> \n" +
                    "<a> { - e:~  . }"
     val strRDF = "<x> <http://ex.org/p> 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate with a qualified stem ") {
     val strShape = "prefix e: <http://example.org/> \n" +
                    "<a> { e:~  . }"
     val strRDF = "prefix e: <http://example.org/> \n" + 
                  "<x> e:p 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate with a stem ") {
     val strShape = "<a> { <http://example.org/>~  . }"
     val strRDF = "<x> <http://example.org/p> 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate a|(b) -> b = ALL") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 		"prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 		"<a> { :a xsd:integer | ( :b xsd:integer ) }"
     val strRDF = "prefix : <http://example.org/>\n" +
    		 	  "<x> :b 1 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate a|(b,c) -> b,c = ALL") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 		"prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 		"<a> { :a xsd:integer | ( :b xsd:integer , :c xsd:integer ) }"
     val strRDF = "prefix : <http://example.org/>\n" +
    		 	  "<x> :b 1; :c 2 ."
     val schema = Schema.fromString(strShape).get._1
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate a|b -> b = ALL") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 		"prefix xsd:  <http://www.w3.org/2001/XMLSchema#>\n" +
    		 		"<a> { :a xsd:integer | :b xsd:integer }"
     val strRDF = "prefix : <http://example.org/>\n" +
    		 	  "<x> :b 1 ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

  it("should validate recursion") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 		"<a> { :a @<a> }"
     val strRDF = "prefix : <http://example.org/>\n" +
    		 	  "<x> :a <x> ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

    it("should validate any") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 		"<a> { . . * }"
     val strRDF = "prefix : <http://example.org/>\n" +
    		 	  "<x> :a 1 ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

    it("should validate any 2") {
     val strShape = "prefix :     <http://example.org/>\n" +
    		 		"<a> { . . * , :a (1)}"
     val strRDF = "prefix : <http://example.org/>\n" +
    		 	  "<x> :a 1, 2 ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     result.isValid should be(true)
   }

    it("should validate collection") {
     val strShape = "prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 		"<li> { rdf:rest (rdf:nil) | rdf:first . , rdf:rest @<li> }"
     val strRDF = "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 	  "<x> rdf:rest rdf:nil ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

    it("should validate collection 2") {
     val strShape = "prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 		"<li> { rdf:rest (rdf:nil) | rdf:first . , rdf:rest @<li> }"
     val strRDF = "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 	  "<x1> rdf:rest rdf:nil ." +
    		 	  "<x2> rdf:first 1 ; rdf:rest <x1> ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x2"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

    it("should validate collection 3") {
     val strShape = "prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 		"<li> { rdf:rest (rdf:nil) | rdf:first . , rdf:rest @<li> }" +
    		 		"<a> { <p> @<li> } "
     val strRDF = "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 	  "<x1> rdf:rest rdf:nil ." +
    		 	  "<x2> rdf:first 1 ; rdf:rest <x1> ." +
    		 	  "<x> <p> <x2> ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     result.isValid should be(true)
   }

    it("should validate collection with 1 element") {
     val strShape = "prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 		"prefix xsd:     <http://www.w3.org/2001/XMLSchema#>\n" +
    		 		"<li> { rdf:first xsd:integer, ( rdf:rest (rdf:nil) | rdf:rest @<listOfInt>) } " +
    		 		"<a> { <p> @<li> } "
     val strRDF = "<x> <p> ( 1 ) ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     info("RDF:\n" + rdf.serialize())
     result.isValid should be(true)
   }

    it("should validate collection with 2 elements") {
     val strShape = "prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 		"prefix xsd:     <http://www.w3.org/2001/XMLSchema#>\n" +
    		 		"<listOfInt> { rdf:first xsd:integer, ( rdf:rest (rdf:nil) | rdf:rest @<listOfInt>) } " +
    		 		"<a> { <p> @<listOfInt> } "
     val strRDF = "<x> <p> ( 1 2 ) ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     info("RDF:\n" + rdf.serialize())
     result.isValid should be(true)
   }

    it("should validate collection 5") {
     val strShape = "prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 		"prefix xsd:     <http://www.w3.org/2001/XMLSchema#>\n" +
    		 		"<li> { rdf:first xsd:integer, ( rdf:rest (rdf:nil) | rdf:rest @<listOfInt>) } " +
    		 		"<a> { <p> @<li> } "
     val strRDF = "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
    		 	  "_:1 rdf:first 1 ; rdf:rest rdf:nil ." +
    		 	  "<x> <p> _:1 ."
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     info("Result:\n" + result.toList.toString)
     info("RDF:\n" + rdf.serialize())
     result.isValid should be(true)
   }
    
   it("should validate anon reference") {
     val strShape = "<a> { <p> @<b> }\n" +
    		 		"<b> { <q> . }"
     val strRDF = "<x> <p> _:1 .\n" + 
    		 	  "_:1 <q> 1 . "
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     result.isValid should be(true)
   }
  
   it("should validate iri ref") {
     val strShape = "<a> { <p> iri }\n" 
     val strRDF = "<x> <p> <q> .\n" 
     val schema = Schema.fromString(strShape).get._1
     info("Schema: " + schema)
     val rdf = RDFTriples.parse(strRDF).get
     val result = Schema.matchSchema(IRI("x"), rdf, schema)
     result.isValid should be(true)
   }
}
 
 
