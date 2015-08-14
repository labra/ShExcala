package es.weso.shacl

import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import es.weso.monads.Result._
import es.weso.monads._
import util._

class MatchTriplesSpec
    extends FunSpec
    with ShaclValidator
    with Matchers
    with Checkers {

  describe("matchLiteralDatatype") {
    it("Should match literal Datatype") {
      val obj: RDFNode = IntegerLiteral(23)
      val ld : LiteralDatatype = LiteralDatatype(xsd_integer,Seq())
      val ctx = Context.emptyContext
      matchLiteralDatatype(obj, ld, ctx).isValid should be(true)
  }
  }
  
 describe("allTriplesWithSamePredicateMatch") {
   it("allTriplesWithSamePredicateMatch. A triple with different predicate passes") {
      val t1 = RDFTriple(IRI("a"),IRI("c"),IRI("x"))
      val ts: Set[RDFTriple] = Set(t1)
      val tc = TripleConstraintCard(None, IRI("b"), IRIKind(List()),defaultCardinality)
      val ctx = Context.emptyContext
      val expected = Pass(typing = Typing.emptyTyping, checked = Set(), remaining = ts,pending=Set())
      allTriplesWithSamePredicateMatch(ts,tc,ctx,false).run should be (Success(Stream(expected)))
    }
   it("allTriplesWithSamePredicateMatch. A triple with the same predicate that matches pass") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("x"))
      val ts: Set[RDFTriple] = Set(t1)
      val tc = TripleConstraintCard(None, IRI("b"), iriKind,defaultCardinality)
      val ctx = Context.emptyContext
      val expected = Pass(typing = Typing.emptyTyping, checked = Set(), remaining = ts,pending=Set())
      allTriplesWithSamePredicateMatch(ts,tc,ctx,false).run should be (Success(Stream(expected)))
    }
   it("allTriplesWithSamePredicateMatch. A triple with the same predicate that doesn't match fails") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),BNodeId("x"))
      val ts: Set[RDFTriple] = Set(t1)
      val tc = TripleConstraintCard(None, IRI("b"), iriKind,defaultCardinality)
      val ctx = Context.emptyContext
      val expected = Pass(typing = Typing.emptyTyping, checked = Set(), remaining = ts,pending=Set())
      allTriplesWithSamePredicateMatch(ts,tc,ctx,false).run should be (Success(Stream()))
    }
  }


  describe("matchValueConstr") {
    it("Should match valueConstr with a value set") {
      val obj: RDFNode = IRI("b")
      val vs = ValueSet(Seq(
          ValueIRI(IRI("a")),
          ValueIRI(IRI("b")),
          ValueIRI(IRI("c"))
          ))
      val ctx = Context.emptyContext
      matchValueConstr(obj, vs, ctx).isValid should be(true)
    }
    
  }

  describe("matchTriplesShapeExpr") {
    it("Should match empty triple with emptyRule") {
      val ts: Set[RDFTriple] = Set()
      val shape : ShapeExpr = EmptyShape 
      val ctx = Context.emptyContext
      val expected = Pass(typing = Typing.emptyTyping, checked = Set(), remaining = ts,pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }
    
    it("Should match single triple with tripleConstraint") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("c"))
      val ts: Set[RDFTriple] = Set(t1)
      val shape : ShapeExpr = TripleConstraintCard(None, IRI("b"), iriKind, defaultCardinality)
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }
    
    it("Should not match single triple with tripleConstraint") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),BNodeId("_1"))
      val ts: Set[RDFTriple] = Set(t1)
      val shape : ShapeExpr = TripleConstraintCard(None, IRI("b"), iriKind, defaultCardinality)
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(),
            pending=Set())
      fails_matchTriplesShapes(ts, shape, expected,false)
    }
    
    
    it("Should match group of two triples") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("x"))
      val t2 = RDFTriple(IRI("a"),IRI("c"),IRI("x"))
      val ts: Set[RDFTriple] = Set(t1,t2)
      val shape : ShapeExpr = 
        GroupShape(None,
            List(TripleConstraintCard(None, IRI("b"), iriKind, defaultCardinality)
                ,TripleConstraintCard(None, IRI("c"), iriKind, defaultCardinality)
                )
        )
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1,t2), 
            remaining = Set(),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }

    it("Should not match group of two triples if first doesn't match") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),BNodeId("_"))
      val t2 = RDFTriple(IRI("a"),IRI("c"),IRI("x"))
      val ts: Set[RDFTriple] = Set(t1,t2)
      val shape : ShapeExpr = 
        GroupShape(None,
            List(TripleConstraintCard(None, IRI("b"), iriKind, defaultCardinality)
                ,TripleConstraintCard(None, IRI("c"), iriKind, defaultCardinality)
                )
        )
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1,t2), 
            remaining = Set(),
            pending=Set())
      fails_matchTriplesShapes(ts, shape, expected)
    }
  
    it("Should not match group of two triples if second doesn't match") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),BNodeId("_"))
      val t2 = RDFTriple(IRI("a"),IRI("c"),IRI("x"))
      val ts: Set[RDFTriple] = Set(t1,t2)
      val shape : ShapeExpr = 
        GroupShape(None,
            List(TripleConstraintCard(None, IRI("b"), bnodeKind, defaultCardinality)
                ,TripleConstraintCard(None, IRI("c"), bnodeKind, defaultCardinality)
                )
        )
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1,t2), 
            remaining = Set(),
            pending=Set())
      fails_matchTriplesShapes(ts, shape, expected)
    }
    
    it("Should match with remaining ") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("x"))
      val t2 = RDFTriple(IRI("a"),IRI("c"),IRI("y"))
      val ts: Set[RDFTriple] = Set(t1,t2)
      val shape : ShapeExpr =
        TripleConstraintCard(None, IRI("b"), iriKind, plus)
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected,false)
    }
    
    it("Should match with remaining - reverse order") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("x"))
      val t2 = RDFTriple(IRI("a"),IRI("c"),IRI("y"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        TripleConstraintCard(None, IRI("b"), iriKind, plus)
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }

    it("Should not match if matchAny extra") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("x"))
      val t2 = RDFTriple(IRI("a"),IRI("b"),BNodeId("_"))
      val ts: Set[RDFTriple] = Set(t1,t2)
      val shape : ShapeExpr = 
        TripleConstraintCard(None, IRI("b"), iriKind, defaultCardinality)
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      fails_matchTriplesShapes(ts, shape, expected)
    }

    it("Should not match if matchAny extra with plus and reverse order") {
      val t1 = RDFTriple(IRI("a"),IRI("b"),IRI("x"))
      val t2 = RDFTriple(IRI("a"),IRI("b"),BNodeId("_"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        TripleConstraintCard(None, IRI("b"), iriKind, plus)
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      fails_matchTriplesShapes(ts, shape, expected)
    }

    it("Should match oneOf (a,d) - oneOf(a,b)") {
      val t1 = RDFTriple(IRI("x"),IRI("a"),IRI("y"))
      val t2 = RDFTriple(IRI("x"),IRI("c"),IRI("z"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        OneOf(None,
            List(TripleConstraintCard(None, IRI("a"), iriKind, plus),
                 TripleConstraintCard(None, IRI("b"), iriKind, plus))
        )
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }
    it("Should match xor (a,d) - (a xor b)") {
      val t1 = RDFTriple(IRI("x"),IRI("a"),IRI("y"))
      val t2 = RDFTriple(IRI("x"),IRI("c"),IRI("z"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        XOr(None,TripleConstraintCard(None, IRI("a"), iriKind, plus),
                 TripleConstraintCard(None, IRI("b"), iriKind, plus))
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }
    it("Should not match xor (a,b) - (a xor b)") {
      val t1 = RDFTriple(IRI("x"),IRI("a"),IRI("y"))
      val t2 = RDFTriple(IRI("x"),IRI("b"),IRI("z"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        XOr(None,TripleConstraintCard(None, IRI("a"), iriKind, plus),
                 TripleConstraintCard(None, IRI("b"), iriKind, plus))
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      fails_matchTriplesShapes(ts, shape, expected)
    }
    it("Should match someOf (a,d) - someOf(a,b)") {
      val t1 = RDFTriple(IRI("x"),IRI("a"),IRI("y"))
      val t2 = RDFTriple(IRI("x"),IRI("c"),IRI("z"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        SomeOf(None,
            List(TripleConstraintCard(None, IRI("a"), iriKind, plus),
                 TripleConstraintCard(None, IRI("b"), iriKind, plus)))
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }
    it("Should match or (a,d) - (a or b)") {
      val t1 = RDFTriple(IRI("x"),IRI("a"),IRI("y"))
      val t2 = RDFTriple(IRI("x"),IRI("c"),IRI("z"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        Or(None,TripleConstraintCard(None, IRI("a"), iriKind, plus),
                TripleConstraintCard(None, IRI("b"), iriKind, plus))
      val expected = 
        Pass(typing = Typing.emptyTyping, 
            checked = Set(t1), 
            remaining = Set(t2),
            pending=Set())
      matchesTriplesShapes(ts, shape, expected)
    }
    it("Should match someOf (a,b) - someOf(a,b)") {
      val t1 = RDFTriple(IRI("x"),IRI("a"),IRI("y"))
      val t2 = RDFTriple(IRI("x"),IRI("b"),IRI("z"))
      val ts: Set[RDFTriple] = Set(t2,t1)
      val shape : ShapeExpr = 
        SomeOf(None,
            List(TripleConstraintCard(None, IRI("a"), iriKind, plus),
                 TripleConstraintCard(None, IRI("b"), iriKind, plus)))
      val e1 = Pass(typing = Typing.emptyTyping,checked = Set(t1),remaining = Set(t2),pending=Set())
      val e2 = Pass(typing = Typing.emptyTyping,checked = Set(t2),remaining = Set(t1),pending=Set())
      val expected = Set(e1,e2)
      val ctx = Context.emptyContext      
      matchTriplesShapeExpr(ts, shape, ctx).run.get.toSet should be(expected)
    }
}
  
  
  def matchesTriplesShapes(
      ts: Set[RDFTriple], 
      shape: ShapeExpr, 
      expectedState: ValidationState,
      withTrace: Boolean = false): Unit = {
    val ctx = Context.emptyContext
    val result: Result[ValidationState] = for {
      _ <- setTrace(withTrace)
      r <- matchTriplesShapeExpr(ts,shape,ctx)
    } yield r
    
    if (result.isValid) {
      val sol1 = result.run.get.head
      if (sol1 != expectedState) {
        info("Different than expected ")
        info("Sol1    : " + sol1)
        info("Expected: " + expectedState)
      }
      sol1 should be(expectedState)
    } else {
      info("matching triples: " + ts + " with " + shape)
      fail("Result " + result + " is not valid")
    }
  }
  
  def fails_matchTriplesShapes(
      ts: Set[RDFTriple], 
      shape: ShapeExpr, 
      expectedState: ValidationState,
      withTrace: Boolean = false): Unit = {
    val ctx = Context.emptyContext
    val result: Result[ValidationState] = for {
      _ <- setTrace(withTrace)
      r <- matchTriplesShapeExpr(ts,shape,ctx)
    } yield r
    if (result.isValid) {
      info("Result valid but should fail")
      info("Result: " + result)
    }
    result.isFailure should be(true)
  }
  
}

