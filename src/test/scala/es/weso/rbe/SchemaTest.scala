package es.weso.rbe

import org.scalatest.{ Pending => ScalaTestPending, _ }
import es.weso.collection._
import util._
import es.weso.rbe._
import StringGraph._

class SchemaTest extends FunSpec with Matchers with TryValues {
  
  def any: NodeShape[String,String,Err] = NodeShape.any
  
  def ref(n: Int) = ConstraintRef(value = n)

  
  describe("Candidates of :a int") {

      // S { :a int }
      val schema: Schema[String, String, String, Err] =
        Schema(
            m = Map("S" -> Shape.empty.copy(
                rbe = Symbol((DirectEdge("a"),integer),1,1))) 
              ,
            ignored = Seq()
            )

      val sorbe = Symbol(ref(1), 1, 1)

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer),
        edges = Map(DirectEdge("a") -> Set(ref(1))),
        elems = 1)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Candidates of (:a 1) should be ok") {
        val expected = Seq(Seq(Pos(ConstraintRef(1),("x","a","1"),DirectEdge("a"))))
        val cs = schema.candidates(table, "x", Seq(Direct("a","1")))
        cs should be(expected)
      }
      
      it("Candidates of (:a foo) should be negative") {
        val cs = schema.candidates(table, "x", Seq(Direct("a","foo")))
        val c = cs.head.head
        c.sign should be(-1)
      } 
    }

 /*   describe("Calculate table") {
    it("Creates a table with an And") {
      val schema: Schema[String, String, String, Err] =
        Schema(
          Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1)))))
      val sorbe = And(Symbol(ref(1), 1, 1), Symbol(ref(2), 1, 1))
      val table = Table(
        constraints = Map(ref(1) -> Ref("t1"), ref(2) -> Ref("t2")),
        edges = Map("a" -> Set(ref(1)), "b" -> Set(ref(2))),
        elems = 2)
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }

    it("Creates a table with an And and several candidates for the same symbol") {
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("a", Ref("t2"))), 1, 1)))))
      val sorbe = And(Symbol(ref(1), 1, 1), Symbol(ref(2), 1, 1))
      val table = Table(
        constraints = Map(ref(1) -> Ref("t1"), ref(2) -> Ref("t2")),
        edges = Map("a" -> Set(ref(1), ref(2))),
        elems = 2)
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }

    it("Calculates table with an And and an Or") {
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", Ref("t1"))), 1, 1),
          Or(Symbol((("b", Ref("t2"))), 1, 1), Symbol((("a", Ref("t2"))), 1, 1))))))

      val sorbe = And(Symbol(ref(1), 1, 1), Or(Symbol(ref(2), 1, 1), Symbol(ref(3), 1, 1)))
      val table = Table(
        constraints = Map(ref(1) -> Ref("t1"), ref(2) -> Ref("t2"), ref(3) -> Ref("t2")),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }

    it("Calculates table combining Ors, Ands and Plus") {
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", isA)), 1, 1),
          Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

      val sorbe = And(Symbol(ref(1), 1, 1), Or(Plus(Symbol(ref(2), 1, 1)), Symbol(ref(3), 1, 1)))
      val table = Table(
        constraints = Map(ref(1) -> isA, ref(2) -> any, ref(3) -> any),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }
  }

  describe("Calculate candidates") {
    describe("Possible candidates of :a int, (:b any + | :a any)") {

      // S { :a int, (:b any + | :a any) }
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
          Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

      // 1, (2+|3)
      val sorbe = And(Symbol(ref(1), 1, 1), Or(Plus(Symbol(ref(2), 1, 1)), Symbol(ref(3), 1, 1)))

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer, ref(2) -> any, ref(3) -> any),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Calculates candidates of :a 1") {
        assertResult(Seq(Pos(ref(1)), Pos(ref(3)))) { schema.possibleCandidates(table, ("a", "1")) }
      }

      it("Calculates candidates of :a x") {
        assertResult(Seq(Neg(ref(1)), Pos(ref(3)))) { schema.possibleCandidates(table, ("a", "x")) }
      }
      it("Calculates candidates of :b 1") {
        assertResult(Seq(Pos(ref(2)))) { schema.possibleCandidates(table, ("b", "1")) }
      }
    }

    describe("Candidates of :a int, (:b any + | :a any)") {

      // S { :a int, (:b any + | :a any) }
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
          Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

      // 1, (2+|3)
      val sorbe = And(Symbol(ref(1), 1, 1), Or(Plus(Symbol(ref(2), 1, 1)), Symbol(ref(3), 1, 1)))

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer, ref(2) -> any, ref(3) -> any),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("Candidates of (:a 1,:a 2)") {
        assertResult(Seq(Seq(Pos(ref(1)), Pos(ref(3))), Seq(Pos(ref(1)), Pos(ref(3))))) {
          schema.candidates(table, Seq(("a", "1"), ("a", "2")))
        }
      }

      it("Candidates of (:a 1,:a x)") {
        assertResult(Seq(
          Seq(Pos(ref(1)), Pos(ref(3))),
          Seq(Neg(ref(1)), Pos(ref(3))))) {
          schema.candidates(table, Seq(("a", "1"), ("a", "x")))
        }
      }

      it("Candidates of (:a 1, :b 1, :c 1)") {
        assertResult(Seq(
          Seq(Pos(ref(1)), Pos(ref(3))),
          Seq(Pos(ref(2))),
          Seq())) {
          schema.candidates(table, Seq(("a", "1"), ("b", "1"), ("c", "1")))
        }
      }
    }


    describe("zip candidates of :a int, (:b any + | :a any)") {

      // S { :a int, (:b any + | :a any) }
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
          Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

      // 1, (2+|3)
      val sorbe = And(Symbol(ref(1), 1, 1), Or(Plus(Symbol(ref(2), 1, 1)), Symbol(ref(3), 1, 1)))

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer, ref(2) -> any, ref(3) -> any),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("zips candidates of (:a 1,:a 2)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Pos(ref(1)))),
          Seq(Some(Pos(ref(1))), Some(Pos(ref(3)))),
          Seq(Some(Pos(ref(3))), Some(Pos(ref(1)))),
          Seq(Some(Pos(ref(3))), Some(Pos(ref(3)))))) {
          schema.zipCandidates(table, Seq(("a", "1"), ("a", "2")))
        }
      }

      it("zips Candidates of (:a 1,:a x)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Neg(ref(1)))),
          Seq(Some(Pos(ref(1))), Some(Pos(ref(3)))),
          Seq(Some(Pos(ref(3))), Some(Neg(ref(1)))),
          Seq(Some(Pos(ref(3))), Some(Pos(ref(3)))))) {
          schema.zipCandidates(table, Seq(("a", "1"), ("a", "x")))
        }
      }

      it("zips Candidates of (:a 1, :b 1, :c 1)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Pos(ref(2))), None),
          Seq(Some(Pos(ref(3))), Some(Pos(ref(2))), None))) {
          schema.zipCandidates(table, Seq(("a", "1"), ("b", "1"), ("c", "1")))
        }
      }
    }

    describe("filter candidates of :a int, (:b any + | :a any)") {

      // S { :a int, (:b any + | :a any) }
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
          Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

      // 1, (2+|3)
      val sorbe: Sorbe[ConstraintRef] = And(Symbol(ref(1), 1, 1), Or(Plus(Symbol(ref(2), 1, 1)), Symbol(ref(3), 1, 1)))

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer, ref(2) -> any, ref(3) -> any),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

      it("filter candidates of (:a 1,:a 2)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Pos(ref(3)))),
          Seq(Some(Pos(ref(3))), Some(Pos(ref(1)))))) {
          schema.filterCandidates(table, Seq(("a", "1"), ("a", "2")), sorbe,true)
        }
      }

      it("filter Candidates of (:a 1,:a x)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Pos(ref(3)))))) {
          schema.filterCandidates(table, Seq(("a", "1"), ("a", "x")), sorbe, true)
        }
      }

      it("filter Candidates of (:a 1,:b x)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Pos(ref(2)))))) {
          schema.filterCandidates(table, Seq(("a", "1"), ("b", "x")), sorbe, true)
        }
      }

      it("filter Candidates of (:a 1, :b x, :c 1)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), Some(Pos(ref(2))), None))) {
          schema.filterCandidates(table, Seq(("a", "1"), ("b", "x"), ("c", "1")), sorbe, true)
        }
      }

      it("filter Candidates of (:a 1, :c 1, :b x)") {
        assertResult(Seq(
          Seq(Some(Pos(ref(1))), None, Some(Pos(ref(2)))))) {
          schema.filterCandidates(table, Seq(("a", "1"), ("c", "1"), ("b", "x")), sorbe, true)
        }
      }
    }

    describe("matching against :a int, (:b any + | :a any)") {

      // S { :a int, (:b any + | :a any) }
      val schema: Schema[String, String, String, Err] =
        Schema(Map("S" -> Shape.empty.copy(rbe = And(Symbol((("a", integer)), 1, 1),
          Or(Plus(Symbol((("b", any)), 1, 1)), Symbol((("a", any)), 1, 1))))))

      // 1, (2+|3)
      val sorbe = And(Symbol(ref(1), 1, 1), Or(Plus(Symbol(ref(2), 1, 1)), Symbol(ref(3), 1, 1)))

      val table: Table[String, String, String, Err] = Table(
        constraints = Map(ref(1) -> integer, ref(2) -> any, ref(3) -> any),
        edges = Map("a" -> Set(ref(1), ref(3)), "b" -> Set(ref(2))),
        elems = 3)

      it("Compares table with expected table") {
        compareResults(schema.mkTable("S"), Success((table, sorbe)))
      }

/*      it("finds matching with no pending values with :a 1. :b 1") {
        assertResult(true) {
          schema.matchNoPending(table, Seq(("a", "1"), ("b", "1")), sorbe, true)
        }
      }*/
    }
  }

  describe("filter candidates of :a int +") {

    // S { :a int + }
    val schema: Schema[String, String, String, Err] =
      Schema(Map("S" -> Shape.empty.copy(rbe = Plus(Symbol((("a", integer)), 1, 1)))))

    // 1+
    val sorbe = Plus(Symbol(ref(1), 1, 1))

    val table: Table[String, String, String, Err] = Table(
      constraints = Map(ref(1) -> integer),
      edges = Map("a" -> Set(ref(1))),
      elems = 1)

    it("Compares table with expected table") {
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }

    it("filter candidates of (:a 1,:a 2)") {
      assertResult(Seq(
        Seq(Some(Pos(ref(1))), Some(Pos(ref(1)))))) {
        schema.filterCandidates(table, Seq(("a", "1"), ("a", "2")), sorbe, true)
      }
    }

    it("filter candidates of (:a 1, :a 2, :a x)") {
      assertResult(Seq()) {
        schema.filterCandidates(table, Seq(("a", "1"), ("a", "2"), ("a", "x")), sorbe, true)
      }
    }
  }

  describe("pending candidates of <S> { :a @<T> +} <T> { :b . } ") {

    // S { :a @t + }
    val schema: Schema[String, String, String, Err] =
      Schema(Map(
        "S" -> Shape.empty.copy(rbe = Plus(Symbol((("a", Ref("T"))), 1, 1))),
        "T" -> Shape.empty.copy(rbe = Symbol((("b", any)), 1, 1))))

    // 1+
    val sorbe = Plus(Symbol(ref(1), 1, 1))

    val table: Table[String, String, String, Err] = Table(
      constraints = Map(ref(1) -> Ref("T")),
      edges = Map("a" -> Set(ref(1))),
      elems = 1)

    it("Compares table with expected table") {
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }

    it("filter candidates of (:a :x,:a y)") {
      assertResult(Seq(
        Seq(Some(Pending(ref(1), "x", "T")), Some(Pending(ref(1), "y", "T"))))) {
        schema.filterCandidates(table, Seq(("a", "x"), ("a", "y")), sorbe, true)
      }
    }
  }

  describe("Test EXTRAs") {
    it("Check a shape with an extra") {
      val schema: Schema[String, String, String, Err] =
        Schema(
         Map("S" -> Shape.empty.copy(
              rbe = And(Symbol((("a", Ref("t1"))), 1, 1), Symbol((("b", Ref("t2"))), 1, 1)),
              extras = Seq("a")
              )
            )
        )
      val sorbe = And(And(Symbol(ref(1), 1, 1), Symbol(ref(2), 1, 1)),Symbol(ref(3),0,Unbounded))
      val table = Table(
        constraints = Map(ref(1) -> Ref("t1"), ref(2) -> Ref("t2"), ref(3) -> any),
        edges = Map("a" -> Set(ref(1),ref(3)), "b" -> Set(ref(2))),
        elems = 3)
      compareResults(schema.mkTable("S"), Success((table, sorbe)))
    }
  } */
  def compareResults[A](s1: A, s2: A) = {
    if (s1 !== s2) {
      fail(s"Values are different\n$s1\n$s2")
    }
  }
}