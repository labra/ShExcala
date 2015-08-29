package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._
import util._
import SESchema._

class SESchemaTest extends FunSpec with Matchers with TryValues {

  object StringPreds {
    lazy val isA: Pred[String] = Pred("isA", _ == "a")
    lazy val integer: Pred[String] = Pred("int", _.matches("""\d+"""))
    lazy val any: Pred[String] = Pred("any", _ => true)
  }

  describe("Compare string preds") {
    import StringPreds._
    val x = any
    val y = any
    x should be(any)
  }

  describe("Define graphs") {

    // Example graphs from paper

    val g0: Graph[String, String] = GraphMap(
      Map("n0" -> List(("a", "n1"), ("b", "n2"), ("a", "n3")),
        "n1" -> List(("b", "n2"), ("a", "n3")),
        "n2" -> List(("b", "n4")),
        "n4" -> List(("c", "n1"))))

    val g1: Graph[String, String] = GraphMap(
      Map("n0" -> List(("a", "n1")),
        "n1" -> List(("b", "n2"), ("c", "n3")),
        "n2" -> List(("c", "n3"))))

    val g2 = GraphMap(
      Map("n0" -> List(("a", "n1")),
        "n1" -> List(("b", "n1"), ("c", "n2"))))

    val s0 =
      Schema(
        Map("t0" -> And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1)),
          "t1" -> Star(Or(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1))),
          "t2" -> Or(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t1"))), 1, 1))))

    val s1 =
      Schema(
        Map("t0" -> Symbol((("a", Ref("t1"))), 1, 1),
          "t1" -> And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1)),
          "t2" -> And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1)),
          "t3" -> Empty))

    import StringPreds._

    val s2: Schema[String, String, String] =
      Schema(
        Map("t0" -> Symbol((("a", integer)), 1, 1),
          "t1" -> And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1)),
          "t2" -> And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1)),
          "t3" -> Empty))

    val typing2: PosNegTyping[String, String] =
      PosNegTyping.fromPosMap(
        Map("n0" -> Set("t0"),
          "n1" -> Set("t1", "t2"),
          "n2" -> Set("t3")))

    it("can define some graph") {

      g0.out("n0") should be(List(("a", "n1"), ("b", "n2"), ("a", "n3")))

    }

    describe("Basic matchings from paper") {
      matchesNodeLabel("n1", "t2", g2, s2, typing2)
    }

    describe("Calculate table") {
      //      val rbe: Sorbe[String,String,NodeShape[String,String]]
      it("Creates a table with an And") {
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1))))
        val sorbe = And(Symbol(1, 1, 1), Symbol(2, 1, 1))
        val table = Table(
          constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2")),
          edges = Map("a" -> Set(1), "b" -> Set(2)),
          elems = 2)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Creates a table with an And and several candidates for the same symbol") {
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("a", Ref("t2"))), 1, 1))))
        val sorbe = And(Symbol(1, 1, 1), Symbol(2, 1, 1))
        val table = Table(
          constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2")),
          edges = Map("a" -> Set(1, 2)),
          elems = 2)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Calculates table with an And and an Or") {
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", Ref("t1"))), 1, 1),
            Or(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("a", Ref("t2"))), 1, 1)))))

        val sorbe = And(Symbol(1, 1, 1), Or(Symbol(2, 1, 1), Symbol(3, 1, 1)))
        val table = Table(
          constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2"), 3 -> Ref("t2")),
          edges = Map("a" -> Set(1, 3), "b" -> Set(2)),
          elems = 3)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Calculates table combining Ors, Ands and Plus") {
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", isA)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1)))))

        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        val table = Table(
          constraints = Map(1 -> isA, 2 -> any, 3 -> any),
          edges = Map("a" -> Set(1, 3), "b" -> Set(2)),
          elems = 3)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }
    }

    describe("Calculate candidates") {
      describe("Possible candidates of :a int, (:b any + | :a any)") {
        
        // S { :a int, (:b any + | :a any) }
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1)))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String] = Table(
          constraints = Map(1 -> integer, 2 -> any, 3 -> any),
          edges = Map("a" -> Set(1,3), "b" -> Set(2)),
          elems = 3)
          
        it ("Compares table with expected table") { 
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("Calculates candidates of :a 1") {
          assertResult(Seq(Pos(1),Pos(3))) { schema.possibleCandidates(table, ("a","1")) }
        }
          
        it ("Calculates candidates of :a x") {
          assertResult(Seq(Neg(1),Pos(3))) { schema.possibleCandidates(table, ("a","x")) }
        }
        it ("Calculates candidates of :b 1") {
          assertResult(Seq(Pos(2))) { schema.possibleCandidates(table, ("b","1")) }
        }
      }
      
      describe("Candidates of :a int, (:b any + | :a any)") {
        
        // S { :a int, (:b any + | :a any) }
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1)))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String] = Table(
          constraints = Map(1 -> integer, 2 -> any, 3 -> any),
          edges = Map("a" -> Set(1,3), "b" -> Set(2)),
          elems = 3)
          
        it ("Compares table with expected table") { 
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("Candidates of (:a 1,:a 2)") {
          assertResult(Seq(Seq(Pos(1),Pos(3)), Seq(Pos(1),Pos(3)))) { 
            schema.candidates(table, Seq( ("a","1"),  ("a","2") )) }
        }
          
        it ("Candidates of (:a 1,:a x)") {
          assertResult(Seq(
              Seq(Pos(1),Pos(3)), 
              Seq(Neg(1),Pos(3)))) { 
            schema.candidates(table, Seq( ("a","1"),  ("a","x") )) }
        }

        it ("Candidates of (:a 1, :b 1, :c 1)") {
          assertResult(Seq(
              Seq(Pos(1),Pos(3)), 
              Seq(Pos(2)),
              Seq()
              )) { 
            schema.candidates(table, Seq( ("a","1"),  ("b","1"), ("c","1") )) }
        }
      }

      describe("zip candidates of :a int, (:b any + | :a any)") {
        
        // S { :a int, (:b any + | :a any) }
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1)))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String] = Table(
          constraints = Map(1 -> integer, 2 -> any, 3 -> any),
          edges = Map("a" -> Set(1,3), "b" -> Set(2)),
          elems = 3)
          
        it ("Compares table with expected table") { 
         compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("zips candidates of (:a 1,:a 2)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(1))), 
              Seq(Some(Pos(1)),Some(Pos(3))),
              Seq(Some(Pos(3)),Some(Pos(1))), 
              Seq(Some(Pos(3)),Some(Pos(3)))
              )) { 
            schema.zipCandidates(table, Seq( ("a","1"),  ("a","2") )) }
        }
          
        it ("zips Candidates of (:a 1,:a x)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Neg(1))), 
              Seq(Some(Pos(1)),Some(Pos(3))),
              Seq(Some(Pos(3)),Some(Neg(1))),
              Seq(Some(Pos(3)),Some(Pos(3))))) { 
            schema.zipCandidates(table, Seq( ("a","1"),  ("a","x") )) }
        }
        
        it ("zips Candidates of (:a 1, :b 1, :c 1)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(2)),None), 
              Seq(Some(Pos(3)),Some(Pos(2)),None)
              )) { 
            schema.zipCandidates(table, Seq( ("a","1"),  ("b","1"), ("c","1") )) }
        }
      }


      describe("filter candidates of :a int, (:b any + | :a any)") {
        
        // S { :a int, (:b any + | :a any) }
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1)))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String] = Table(
          constraints = Map(1 -> integer, 2 -> any, 3 -> any),
          edges = Map("a" -> Set(1,3), "b" -> Set(2)),
          elems = 3)
          
        it ("Compares table with expected table") { 
         compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("filter candidates of (:a 1,:a 2)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(3))),
              Seq(Some(Pos(3)),Some(Pos(1)))
              )) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("a","2") ), sorbe) }
        }
          
        it ("filter Candidates of (:a 1,:a x)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(3)))
              )) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("a","x") ), sorbe) }
        } 
        
        it ("filter Candidates of (:a 1,:b x)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(2)))
              )) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("b","x") ), sorbe) }
        } 
        
        it ("filter Candidates of (:a 1, :b x, :c 1)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(2)),None)
              )) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("b","x"), ("c","1") ), sorbe) }
        } 

        it ("filter Candidates of (:a 1, :c 1, :b x)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),None,Some(Pos(2)))
              )) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("c","1"), ("b","x") ), sorbe) }
        } 
      }
    
      describe("matching against :a int, (:b any + | :a any)") {
        
        // S { :a int, (:b any + | :a any) }
        val schema: Schema[String, String, String] =
          Schema(Map("S" -> And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1)))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String] = Table(
          constraints = Map(1 -> integer, 2 -> any, 3 -> any),
          edges = Map("a" -> Set(1,3), "b" -> Set(2)),
          elems = 3)
          
        it ("Compares table with expected table") { 
         compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("finds matching with no pending values with :a 1. :b 1") {
          assertResult(true) { 
            schema.matchNoPending(table, Seq( ("a","1"),  ("b","1") ), sorbe) }
        }
      }
    }

    def matchesNodeLabel[Edge, Node, Label](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label], t: PosNegTyping[Node, Label]): Unit = {
      it(s"Matches node $n with label $l in graph ${g} and schema ${s}") {
        s.matchNode(n, l, g) should be(t)
      }
    }

    def compareResults[A](s1: A, s2: A) = {
      if (s1 != s2) {
        fail(s"Values are different\n$s1\n$s2")
      }
    }

  }
}