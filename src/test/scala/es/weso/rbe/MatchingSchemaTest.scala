package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._
import util._
import es.weso.rbe._
import StringGraph._
import es.weso.typing.PosNegTyping

class MatchingSchemaTest extends FunSpec with Matchers with TryValues {
  
  def noTs: Set[(String,String,String)] = Set()

  describe("Graphs from paper") {

    // Example graphs from paper

    val g0: Graph[String, String] = GraphMap(
      Map(
        "n0" -> List(("a", "n1"), ("b", "n2"), ("a", "n3")),
        "n1" -> List(("b", "n2"), ("a", "n3")),
        "n2" -> List(("b", "n4")),
        "n4" -> List(("c", "n1")))
      )

    val g1: Graph[String, String] = GraphMap(
      Map("n0" -> List(("a", "n1")),
        "n1" -> List(("b", "n2"), ("c", "n3")),
        "n2" -> List(("c", "n3"))))

    val g2 = GraphMap(
      Map("n0" -> List(("a", "n1")),
        "n1" -> List(("b", "n1"), ("c", "n2"))))

    val s0 : Schema[String,String,String,Throwable] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("a"), Ref("t1"))), 1, 1), Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1))),
          "t1" -> Shape.empty.copy(rbe = Star(Or(Symbol(((DirectEdge("a"), Ref("t1"))), 1, 1), Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1)))),
          "t2" -> Shape.empty.copy(rbe = Or(Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1), Symbol(((DirectEdge("c"), Ref("t1"))), 1, 1)))),
        Seq())

    val s1 : Schema[String,String,String,Throwable]  =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("t1"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 0, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)),
        Seq())


    val s2: Schema[String, String, String, Throwable] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("t0"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 0, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)),
        Seq())

    val typing2 =
      Seq((PosNegTyping.fromPosMap(
        Map("n0" -> Set("t0"),
          "n1" -> Set("t1", "t2"),
          "n2" -> Set("t3"))),noTs))

    it("can define some graph") {
      g0.out("n0") should be(List(("a", "n1"), ("b", "n2"), ("a", "n3")))
    }

    ignore("Basic matchings from paper") {
      matchesNodeLabel("n0", "t0", g2, s2, typing2)
    }
    
  }
    
  describe("Basic matchings") {
      val s : Schema[String,String,String,Err] = 
        Schema(Map("s" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), integer)), 1, 1))),
        Seq())
      val typing = PosNegTyping.fromPosMap(Map("x" -> Set("s")))
        
      val expected = Seq((typing,Set(("x","a","1"))))
      val g = GraphMap(Map("x" -> List(("a", "1"))))
      matchesNodeLabel("x", "s", g, s, expected)
      
      val g1 = GraphMap(Map("x" -> Seq( ("a", "1"), ("b","1") )))
      val expected1 = Seq((typing,Set(("x","a","1"))))
      matchesNodeLabel("x", "s", g1, s, expected1)

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
      val s : Schema[String,String,String,Err] = 
        Schema(Map("s" -> Shape.empty.copy(rbe = Plus(Symbol(((DirectEdge("a"), integer)), 1, 1)))),
        Seq())
      val typing = PosNegTyping.fromPosMap(Map("x" -> Set("s")))

      val g = GraphMap(Map("x" -> List(("a", "1"))))
      val expected0 = Seq((typing,Set(("x","a","1"))))
      matchesNodeLabel("x", "s", g, s, expected0)
      
      val g1 = GraphMap(Map("x" -> Seq( ("a", "1"), ("b","1") )))
      val expected1 = Seq((typing,Set(("x","a","1"))))
      matchesNodeLabel("x", "s", g1, s, expected1)

      val g2 = GraphMap(Map("x" -> Seq[(String,String)]()))
      noMatchNodeLabel("x", "s", g2, s)
      
      val g3 = GraphMap(Map("x" -> Seq(("b","1"))))
      noMatchNodeLabel("x", "s", g3, s)

      val g4 = GraphMap(Map("x" -> Seq(("b","1"),("c","1"))))
      noMatchNodeLabel("x", "s", g4, s)

      val g5 = GraphMap(Map("x" -> Seq(("a","x"))))
      noMatchNodeLabel("x", "s", g5, s)

      val g7 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"))))
      val expected7 = Seq((typing,Set(("x","a","2"),("x","a","4"))))
      matchesNodeLabel("x", "s", g7, s,expected7)

      val g8 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"),("b","4"))))
      val expected8 = Seq((typing,Set(("x","a","2"),("x","a","4"))))
      matchesNodeLabel("x", "s", g8, s, expected8)
      
      val g9 = GraphMap(Map("x" -> Seq(("a","2"),("a","4"),("a","x"))))
      noMatchNodeLabel("x", "s", g9, s)
    }

  ignore("Matching with Or --- single one to ignore and check this case with more detail later") {
      val s : Schema[String,String,String,Err] = 
        Schema(Map("s" -> Shape.empty.copy(rbe = Or(
            Symbol(((DirectEdge("a"), one)), 1, 1),
            Symbol(((DirectEdge("b"), one)), 1, 1)))),
        Seq())

      val typing = PosNegTyping.fromPosMap(Map("x" -> Set("s")))
      
      // TODO: Check this one
      val g8 = GraphMap(Map("x" -> Seq(("a","1"),("c","4"),("b","4"))))
      val expected8 = Seq((typing,Set(("x","a","1"))))
      matchesNodeLabel("x", "s", g8, s,expected8)
    
  }
  describe("Basic matchings with Or") {
      val s : Schema[String,String,String,Err] = 
        Schema(Map("s" -> Shape.empty.copy(rbe = Or(
            Symbol(((DirectEdge("a"), one)), 1, 1),
            Symbol(((DirectEdge("b"), one)), 1, 1)))),
        Seq())

      val typing = PosNegTyping.fromPosMap(Map("x" -> Set("s")))
      
      val g0 = GraphMap(Map("x" -> Seq(("a", "1"))))
      val expected0 = Seq((typing,Set(("x","a","1"))))
      matchesNodeLabel("x", "s", g0, s, expected0)
      
      val g1 = GraphMap(Map("x" -> Seq( ("b","1") )))
      val expected1 = Seq((typing,Set(("x","b","1"))))
      matchesNodeLabel("x", "s", g1, s, expected1)

      val g2 = GraphMap(Map("x" -> Seq[(String,String)]()))
      val expected2 = Seq((typing,Seq(("x","b","1"))))
      noMatchNodeLabel("x", "s", g2, s)
      
      val g3 = GraphMap(Map("x" -> Seq(("c","1"))))
      noMatchNodeLabel("x", "s", g3, s)

      val g4 = GraphMap(Map("x" -> Seq(("b","1"),("c","1"))))
      val expected4 = Seq((typing,Set(("x","b","1"))))
      matchesNodeLabel("x", "s", g4, s,expected4)

      val g5 = GraphMap(Map("x" -> Seq(("a","x"))))
      noMatchNodeLabel("x", "s", g5, s)

      val g7 = GraphMap(Map("x" -> Seq(("a","1"),("b","1"))))
      noMatchNodeLabel("x", "s", g7, s)

      // g8 moved to a different set so we can ignore it by now
      
      val g9 = GraphMap(Map("x" -> Seq(("a","1"),("b","1"),("a","x"))))
      noMatchNodeLabel("x", "s", g9, s) 
      
    }

  describe("Schema with ref") {

    val g = GraphMap(
      Map("n0" -> List(("a", "n1")),
          "n1" -> List(("b", "n1"), ("c", "n2"))))

    val s: Schema[String, String, String, Throwable] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("t1"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 0, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)),
        Seq())

    val typing0 = PosNegTyping.fromPosMap(
        Map("n1" -> Set("t1", "t2"),
            "n2" -> Set("t3")))
    val expected0 = Seq((typing0,Set(("n1","c","n2"),("n1","b","n1"))))
    matchesNodeLabel("n1", "t1", g, s, expected0)

    val typing1 =
      PosNegTyping.fromPosMap(
        Map("n0" -> Set("t0"),
            "n1" -> Set("t1", "t2"),
            "n2" -> Set("t3")))
    val expected1 = Seq((typing1,Set(("n1","b","n1"),("n0","a","n1"),("n1","c","n2"))))
    matchesNodeLabel("n0", "t0", g, s, expected1)

      
  }

  
    def matchesNodeLabel[Edge, Node, Label,Err](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label,Err], 
      t: Seq[(PosNegTyping[Node, Label],Set[(Node,Edge,Node)])]): Unit = {
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