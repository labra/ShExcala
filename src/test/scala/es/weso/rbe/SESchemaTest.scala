package es.weso.rbe

import org.scalatest.{Pending => ScalaTestPending, _}
import es.weso.collection._
import es.weso.rbe.Interval._
import util._
import SESchema._
import StringGraph._

class SESchemaTest extends FunSpec with Matchers with TryValues {

    describe("Calculate table") {
      it("Creates a table with an And") {
        val schema: Schema[String, String, String,Throwable] =
          Schema(
              Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1)))))
        val sorbe = And(Symbol(1, 1, 1), Symbol(2, 1, 1))
        val table = Table(
          constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2")),
          edges = Map("a" -> Set(1), "b" -> Set(2)),
          elems = 2)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Creates a table with an And and several candidates for the same symbol") {
        val schema: Schema[String, String, String,Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("a", Ref("t2"))), 1, 1)))))
        val sorbe = And(Symbol(1, 1, 1), Symbol(2, 1, 1))
        val table = Table(
          constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2")),
          edges = Map("a" -> Set(1, 2)),
          elems = 2)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Calculates table with an And and an Or") {
        val schema: Schema[String, String, String,Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1),
            Or(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("a", Ref("t2"))), 1, 1))))))

        val sorbe = And(Symbol(1, 1, 1), Or(Symbol(2, 1, 1), Symbol(3, 1, 1)))
        val table = Table(
          constraints = Map(1 -> Ref("t1"), 2 -> Ref("t2"), 3 -> Ref("t2")),
          edges = Map("a" -> Set(1, 3), "b" -> Set(2)),
          elems = 3)
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Calculates table combining Ors, Ands and Plus") {
        val schema: Schema[String, String, String,Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", isA)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

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
        val schema: Schema[String, String, String,Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String,Throwable] = Table(
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
        val schema: Schema[String, String, String,Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String,Throwable] = Table(
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
        val schema: Schema[String, String, String, Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String,Throwable] = Table(
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
        val schema: Schema[String, String, String, Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String,Throwable] = Table(
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
        val schema: Schema[String, String, String, Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
            Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

        // 1, (2+|3)
        val sorbe = And(Symbol(1, 1, 1), Or(Plus(Symbol(2, 1, 1)), Symbol(3, 1, 1)))
        
        val table : Table[String,String,String,Throwable] = Table(
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

      describe("filter candidates of :a int +") {
        
        // S { :a int + }
        val schema: Schema[String, String, String, Throwable] =
          Schema(Map("S" -> Shape.empty.copy(rbe = Plus(Symbol((("a", integer)), 1, 1)))))

        // 1+
        val sorbe = Plus(Symbol(1, 1, 1))
        
        val table : Table[String,String,String,Throwable] = Table(
          constraints = Map(1 -> integer),
          edges = Map("a" -> Set(1)),
          elems = 1)
          
        it ("Compares table with expected table") { 
         compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("filter candidates of (:a 1,:a 2)") {
          assertResult(Seq(
              Seq(Some(Pos(1)),Some(Pos(1)))
              )) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("a","2") ), sorbe) }
        }
        
        it ("filter candidates of (:a 1, :a 2, :a x)") {
          assertResult(Seq()) { 
            schema.filterCandidates(table, Seq( ("a","1"),  ("a","2"), ("a","x") ), sorbe) }
        }
      }

      describe("pending candidates of <S> { :a @<T> +} <T> { :b . } ") {
        
        // S { :a @t + }
        val schema: Schema[String, String, String, Throwable] =
          Schema(Map(
              "S" -> Shape.empty.copy(rbe = Plus(Symbol((("a", Ref("T"))), 1, 1))),
              "T" -> Shape.empty.copy(rbe = Symbol((("b", any)), 1, 1))))

        // 1+
        val sorbe = Plus(Symbol(1, 1, 1))
        
        val table : Table[String,String,String,Throwable] = Table(
          constraints = Map(1 -> Ref("T")),
          edges = Map("a" -> Set(1)),
          elems = 1)
          
        it ("Compares table with expected table") { 
         compareResults(schema.mkTable("S"), Success((table, sorbe)))
        }
        
        it ("filter candidates of (:a :x,:a y)") {
          assertResult(Seq(
              Seq(Some(Pending(1,"x","T")),Some(Pending(1,"y","T")))
              )) { 
            schema.filterCandidates(table, Seq( ("a","x"),  ("a","y") ), sorbe) }
        }
      }
      
    def compareResults[A](s1: A, s2: A) = {
      if (s1 !== s2) {
        fail(s"Values are different\n$s1\n$s2")
      }
    }
  
}