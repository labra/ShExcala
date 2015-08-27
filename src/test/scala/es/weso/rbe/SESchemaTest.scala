package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._
import util._

class SESchemaTest extends FunSpec with Matchers with TryValues {

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
      SESchema(
        Map("t0" -> And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1)),
            "t1" -> Star(Or(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1))),
            "t2" -> Or(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t1"))), 1, 1))
            )) 

    val s1 =
      SESchema(
        Map("t0" -> Symbol((("a", Ref("t1"))), 1, 1),
            "t1" -> And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1)),
            "t2" -> And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1)),
            "t3" -> Empty
            ))
            
    val isNumber : Pred[String] = Pred(_.matches("""\d+"""))
            
    val s2 : SESchema[String,String,String] =
      SESchema(
        Map("t0" -> Symbol((("a", isNumber)), 1, 1),
            "t1" -> And(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("c", Ref("t3"))), 1, 1)),
            "t2" -> And(Symbol((("b", Ref("t2"))), 0, 1), Symbol((("c", Ref("t3"))), 1, 1)),
            "t3" -> Empty
            ))
            
    val typing2 : PosNegTyping[String,String] =
      PosNegTyping.fromPosMap(
          Map("n0" -> Set("t0"),
              "n1" -> Set("t1","t2"),
              "n2" -> Set("t3")))
            
    it("can define some graph") {

      g0.out("n0") should be(List(("a", "n1"), ("b", "n2"), ("a", "n3")))

    }
    
    describe("Basic matchings from paper") {
     matchesNodeLabel("n1","t2",g2,s2,typing2) 
    }
    
    describe("Calculate table") {
//      val rbe: Sorbe[String,String,NodeShape[String,String]]
      it("Creates a table with an And") {
       val schema: SESchema[String,String,String] = 
         SESchema(Map("S" -> And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))),1,1))))
       val sorbe = And(Symbol(1, 1, 1), Symbol(2,1,1))
       val table = Table(
             constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2")), 
             edges = Map("a" -> Set(1), "b" -> Set(2)),
             elems = 2)
        compareResults(schema.mkTable("S"), Success((table,sorbe)))
      }
    }
    
    def matchesNodeLabel[Edge,Node,Label](
        n: Node, 
        l: Label, 
        g: Graph[Edge,Node],
        s : SESchema[Edge,Node,Label], t: PosNegTyping[Node,Label]): Unit = {
      it(s"Matches node $n with label $l in graph ${g} and schema ${s}") {
       s.matchNode(n,l,g) should be(t) 
      }
    }
    

    def compareResults[A](s1: A, s2:A) = {
      if (s1 != s2) {
        fail(s"Values are different\n$s1\n$s2")
      }
    }
    
    
  }
}