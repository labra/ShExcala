package es.weso.rbe

import org.scalatest.{ Pending => ScalaTestPending, _ }
import es.weso.collection._
import util._
import es.weso.rbe._
import StringGraph._
import es.weso.typing._

class ResolveCandidatesTest extends FunSpec with Matchers with TryValues {
  
  def any: NodeShape[String,String,Err] = NodeShape.any
  
  def ref(n: Int) = ConstraintRef(value = n)

    describe("Resolve candidates of :a int") {

      // S { :a int }
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), integer)), 1, 1))))
        
      val graph: Graph[String,String] = GraphMap(Map("x" -> Seq(("a","50"))))

      val sorbe = Symbol(ref(1), 1, 1)

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer),
        edges = Map(DirectEdge("a") -> Set(ref(1))),
        elems = 1)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Matches (x,a,50) with S") {
        val typeRow: TypeRow[String] = TypeRow(pos = Set("S"),Set()) 
        val expectedType : PosNegTyping[String,String] = PosNegTypingAsMap(Map("x" -> typeRow)) 
        assertResult(Success(Seq((expectedType,Seq(("x","a","50")))))) {
          schema.matchNode("x", "S", graph)
        }
      }

    }

    describe("Resolve candidates of S {:a T}, T {:b Int} ") {

      val schema: Schema[String, String, String, Err] =
        Schema(Map(
            "S" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("T"))),1,1)),
            "T" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1))))
        
      val graph: Graph[String,String] = 
        GraphMap(Map(
            "x" -> Seq(("a","y")), 
            "y" -> Seq(("b","51"))
            ))

      val sorbe = Symbol(ref(1), 1, 1)

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> Ref("T")),
        edges = Map(DirectEdge("a") -> Set(ref(1))),
        elems = 1)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Matches (x,a,50) with S") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT: TypeRow[String] = TypeRow(pos = Set("T"),Set())
        val expectedType = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT)) 
        assertResult(Success(Seq((expectedType,Seq(("x","a","y"),("y","b","51")))))) {
          schema.matchNode("x", "S", graph)
        }
      }

    }
    
    describe("Resolve candidates of S {:a T1, :a T2}, T1 {:b Int}, T2 {:b Int} ") {

      // S { :a int, (:b any + | :a any) }
      val schema: Schema[String, String, String, Err] =
        Schema(Map(
            "S" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("a"), Ref("T1"))),1,1),Symbol(((DirectEdge("a"), Ref("T2"))),1,1))),
            "T1" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1)),
            "T2" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1))
            ))
            
      val schemaClosed: Schema[String, String, String, Err] =
        Schema(Map(
            "S" -> Shape.empty.copy(
                rbe = And(Symbol(((DirectEdge("a"), Ref("T1"))),1,1),Symbol(((DirectEdge("a"), Ref("T2"))),1,1)),
                closed = true),
            "T1" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1)),
            "T2" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("b"), integer)), 1, 1))
            ))
            
        
      val g1: Graph[String,String] = 
        GraphMap(Map(
            "x" -> Seq(("a","y"), ("a","z")), 
            "y" -> Seq(("b","51")),
            "z" -> Seq(("b","52"))
            ))
            
      val g2: Graph[String,String] = 
        GraphMap(Map(
            "x" -> Seq(("a","y"), ("x","x"),("a","z")), 
            "y" -> Seq(("b","51")),
            "z" -> Seq(("b","52"))
            ))
            

      val sorbe = And(Symbol(ref(1), 1, 1),Symbol(ref(2), 1, 1))

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> Ref("T1"), ref(2) -> Ref("T2")),
        edges = Map(DirectEdge("a") -> Set(ref(1),ref(2))),
        elems = 2)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Matches (x,a,y)(x,a,z)(y,b,40)(y,b,51) with S") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT1: TypeRow[String] = TypeRow(pos = Set("T1"),Set())
        val typeT2: TypeRow[String] = TypeRow(pos = Set("T2"),Set())
        val expectedType1 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT1, "z" -> typeT2)) 
        val expectedType2 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT2, "z" -> typeT1)) 
        assertResult(Success(Seq(
            (expectedType1,Seq(("x","a","y"),("y","b","51"))),
            (expectedType2,Seq(("x","a","y"),("y","b","51")))
            ))) {
          schema.matchNode("x", "S", g1)
        }
      }
      it("Matches (x,a,y)(x,x,x)(x,a,z)(y,b,40)(y,b,51) with S") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT1: TypeRow[String] = TypeRow(pos = Set("T1"),Set())
        val typeT2: TypeRow[String] = TypeRow(pos = Set("T2"),Set())
        val expectedType1 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT1, "z" -> typeT2)) 
        val expectedType2 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT2, "z" -> typeT1)) 
        assertResult(Success(Seq(
            (expectedType1,Seq(("x","a","y"),("y","b","51"))),
            (expectedType2,Seq(("x","a","y"),("y","b","51")))
            ))) {
          schema.matchNode("x", "S", g2)
        }
      }
      it("Doesn't match (x,a,y)(x,x,x)(x,a,z)(y,b,40)(y,b,51) with S if schemaClosed") {
        val typeS: TypeRow[String] = TypeRow(pos = Set("S"),Set())
        val typeT1: TypeRow[String] = TypeRow(pos = Set("T1"),Set())
        val typeT2: TypeRow[String] = TypeRow(pos = Set("T2"),Set())
        val expectedType1 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT1, "z" -> typeT2)) 
        val expectedType2 = PosNegTypingAsMap(Map("x" -> typeS, "y" -> typeT2, "z" -> typeT1)) 
        assertResult(Success(Seq())) {
          schemaClosed.matchNode("x", "S", g2)
        }
      }

    }
    
  def compareResults[A](s1: A, s2: A) = {
    if (s1 !== s2) {
      fail(s"Values are different\n$s1\n$s2")
    }
  }

}