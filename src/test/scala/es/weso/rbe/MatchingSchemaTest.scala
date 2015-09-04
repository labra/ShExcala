package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._
import util._
import SESchema._
import StringGraph._
import es.weso.typing.PosNegTyping

class MatchingSchemaTest extends FunSpec with Matchers with TryValues {

  describe("Graphs from paper") {

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

    val s0 : Schema[String,String,String,Throwable] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1))),
          "t1" -> Shape.empty.copy(rbe = Star(Or(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1)))),
          "t2" -> Shape.empty.copy(rbe = Or(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t1"))), 1, 1)))))

    val s1 : Schema[String,String,String,Throwable]  =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol((("a", Ref("t1"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)))


    val s2: Schema[String, String, String, Throwable] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol((("a", Ref("t0"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)))

    val typing2 =
      Seq(PosNegTyping.fromPosMap(
        Map("n0" -> Set("t0"),
          "n1" -> Set("t1", "t2"),
          "n2" -> Set("t3"))))

    it("can define some graph") {
      g0.out("n0") should be(List(("a", "n1"), ("b", "n2"), ("a", "n3")))
    }

    describe("Basic matchings from paper") {
      matchesNodeLabel("n0", "t0", g2, s2, typing2)
    }
    
  }
    
  describe("Basic matchings") {
      val s : Schema[String,String,String,Throwable] = Schema(Map("s" -> Shape.empty.copy(rbe = Symbol((("a", integer)), 1, 1))))
      val g = GraphMap(Map("x" -> List(("a", "1"))))
      val t = Seq(PosNegTyping.fromPosMap(Map("x" -> Set("s"))))
      matchesNodeLabel("x", "s", g, s, t)
      
      val g1 = GraphMap(Map("x" -> Seq( ("a", "1"), ("b","1") )))
      matchesNodeLabel("x", "s", g1, s, t)

      val g2 = GraphMap(Map("x" -> Seq[(String,String)]()))
      noMatchNodeLabel("x", "s", g2, s)
      
      val g3 = GraphMap(Map("x" -> Seq(("b","1"))))
      noMatchNodeLabel("x", "s", g3, s)

      val g4 = GraphMap(Map("x" -> Seq(("b","1"),("c","1"))))
      noMatchNodeLabel("x", "s", g4, s)

      val g5 = GraphMap(Map("x" -> Seq(("a","x"))))
      noMatchNodeLabel("x", "s", g5, s)

      val g7 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"))))
      noMatchNodeLabel("x", "s", g7, s)
  }
  
  describe("Basic matchings with plus") {
      val s : Schema[String,String,String,Throwable] = Schema(Map("s" -> Shape.empty.copy(rbe = Plus(Symbol((("a", integer)), 1, 1)))))
      val g = GraphMap(Map("x" -> List(("a", "1"))))
      val t = Seq(PosNegTyping.fromPosMap(Map("x" -> Set("s"))))
      matchesNodeLabel("x", "s", g, s, t)
      
      val g1 = GraphMap(Map("x" -> Seq( ("a", "1"), ("b","1") )))
      matchesNodeLabel("x", "s", g1, s, t)

      val g2 = GraphMap(Map("x" -> Seq[(String,String)]()))
      noMatchNodeLabel("x", "s", g2, s)
      
      val g3 = GraphMap(Map("x" -> Seq(("b","1"))))
      noMatchNodeLabel("x", "s", g3, s)

      val g4 = GraphMap(Map("x" -> Seq(("b","1"),("c","1"))))
      noMatchNodeLabel("x", "s", g4, s)

      val g5 = GraphMap(Map("x" -> Seq(("a","x"))))
      noMatchNodeLabel("x", "s", g5, s)

      val g7 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"))))
      matchesNodeLabel("x", "s", g7, s,t)

      val g8 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"),("b","4"))))
      matchesNodeLabel("x", "s", g8, s,t)
      
      val g9 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"),("a","x"))))
      noMatchNodeLabel("x", "s", g9, s)
    }


  describe("Schema with ref") {

    val g = GraphMap(
      Map("n0" -> List(("a", "n1")),
          "n1" -> List(("b", "n1"), ("c", "n2"))))

    val s: Schema[String, String, String, Throwable] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol((("a", Ref("t1"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)))

    val expectedTypeN0 =
      Seq(PosNegTyping.fromPosMap(
        Map("n0" -> Set("t0"),
          "n1" -> Set("t1", "t2"),
          "n2" -> Set("t3"))))
          
    val expectedTypeN1 =
      Seq(PosNegTyping.fromPosMap(
        Map("n1" -> Set("t1", "t2"),
            "n2" -> Set("t3"))))
            
      matchesNodeLabel("n1", "t1", g, s, expectedTypeN1)
      matchesNodeLabel("n0", "t0", g, s, expectedTypeN0)

      
  }

  
    def matchesNodeLabel[Edge, Node, Label,Err](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label,Err], t: Seq[PosNegTyping[Node, Label]]): Unit = {
      it(s"Matches node $n with label $l in graph ${g} and schema ${s}") {
        s.matchNode(n, l, g) should be(Success(t))
      }
    }
    
    def noMatchNodeLabel[Edge, Node, Label,Err](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label,Err]): Unit = {
      it(s"Doesn't match node $n with label $l in graph ${g} and schema ${s}") {
        val result = s.matchNode(n, l, g) 
        result match {
          case Success(ls) if (!ls.isEmpty) => fail(s"It matches with: $ls")
          case _ => info("Doesn't match as expected")
        }
      }
    }

    def compareResults[A](s1: A, s2: A) = {
      if (s1 != s2) {
        fail(s"Values are different\n$s1\n$s2")
      }
    }

}