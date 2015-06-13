package es.weso.monads

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.monads.Result._
import Stream._
import util._

class ResultSpec
    extends FunSpec
    with Matchers
    with Checkers {

  describe("a Result") {

    it("Should return a value") {
      val bm: Result[Int] = unit(2)
      bm.run should be(Success(2 #:: Stream.empty))
    }

    it("Should fail") {
      val bm: Result[Int] = failure("example failed")
      bm.run.isFailure should be(true)
    }

    it("Should recover from fail") {
      val bm: Result[Int] =
        failure("example failed").orelse(unit(2))
      bm.run should be(Success(List(2).toStream))
    }

    it("Should be able to use flatMap") {
      val bm: Result[Int] =
        // for (x <- unit(2)) yield (x + 1)  
        unit(2) flatMap { x => unit(x + 1) }

      bm.run should be(Success(List(3).toStream))
    }

    it("Should be able to use for's") {
      val bm: Result[Int] =
        for (x <- unit(2)) yield x + 1

      bm.run should be(Success(List(3).toStream))
    }

    it("Should be able to use for to generate a pair") {
      val bm: Result[(Int, Int)] =
        for (
          x <- unit(2); y <- unit(x + 1)
        ) yield (x, y)

      bm.run should be(Success(List((2, 3)).toStream))
    }

    it("A sequence with a fail...fails") {
      val u1: Result[Int] = unit(1)
      val f: Result[Int] = failure("fail")
      val bm = for (x <- u1; y <- f) yield x + y
      bm.isFailure should be(true)
    }

    describe("pass all") {
      it("should pass all...if all pass") {
        val xs = List(1, 2, 3)
        val current = true
        def eval(x: Int, b: Boolean) =
          if (x > 0 && b) unit(true)
          else failure("x <= 0")
        passAll(xs, current, eval).isValid should be(true)
      }

      it("should not pass all...if one does not pass") {
        val xs = List(1, -2, 3)
        val current = true
        def eval(x: Int, b: Boolean) =
          if (x > 0 && b) unit(true)
          else failure("x <= 0")
        passAll(xs, current, eval).isFailure should be(true)
      }

    }

    describe("pass Some") {

      it("should pass some...if all pass") {
        val xs = List(1, 2, 3)
        def eval(x: Int) =
          if (x > 0) unit(true)
          else failure("x <= 0")
        passSome(xs, eval).isValid should be(true)
      }

      it("should pass some...if one pass") {
        val xs = List(-1, 2, -3)
        def eval(x: Int) =
          if (x > 0) unit(true)
          else failure("x <= 0")
        passSome(xs, eval).isValid should be(true)
      }

      it("should not pass ...if none pass") {
        val xs = List(-1, -2, -3)
        def eval(x: Int) =
          if (x > 0) unit(true)
          else failure("x <= 0")
        passSome(xs, eval).isValid should be(false)
      }

    }

    describe("Or else") {
      it("A sequence with a fail and orelse...recovers") {
        val u1: Result[Int] = unit(1)
        val f: Result[Int] = failure("fail")
        val u2: Result[Int] = unit(2)
        val bm1 = for (x <- u1; y <- f) yield (x + y)
        val bm2 = bm1 orelse u2
        bm1.isFailure should be(true)
        bm2.isFailure should be(false)
        bm2.run should be(Success(List((2)).toStream))
      }

    }

    describe("Merge") {
      it("Should merge two computations") {
        val comp1: Result[Int] = Passed(Stream(1, 2, 3))
        val comp2: Result[Int] = Passed(Stream(4, 5, 6))
        def comb(x: Int, y: Int) = x + y
        merge(comp1, comp2, comb) should be(Passed(Stream(5, 6, 7, 6, 7, 8, 7, 8, 9)))
      }
      it("Should merge a computation with values and the basic computation") {
        val comp1: Result[Int] = Passed(Stream(1, 2, 3))
        val comp2: Result[Int] = Passed(Stream(0))
        def comb(x: Int, y: Int) = x + y
        merge(comp1, comp2, comb) should be(Passed(Stream(1, 2, 3)))
      }
      it("Should merge a computation with values and a Failure computation") {
        val comp1: Result[Int] = Passed(Stream(1, 2, 3))
        val comp2: Result[Int] = Failure("hi")
        def comb(x: Int, y: Int) = x + y
        merge(comp1, comp2, comb) should be(Passed(Stream(1, 2, 3)))
      }

      it("Should merge a computation with values and an empty computation") {
        val comp1: Result[Int] = Passed(Stream(1, 2, 3))
        val comp2: Result[Int] = Passed(Stream(0))
        def comb(x: Int, y: Int) = x + y
        merge(comp1, comp2, comb) should be(Passed(Stream(1, 2, 3)))
      }

    }

    describe("Combine All") {
      it("Should combine two computations") {
        val ls: List[Int] = List(1, 2)
        def eval(n: Int): Result[String] = { Passed({ (for (i <- 0 to n) yield i.toString).toStream }) }
        def comb(x: String, y: String) = x + y
        combineAll(ls, eval, comb) should be(Passed(Stream("00", "01", "02", "10", "11", "12")))
      }

      it("Should combine three computations even if one fails") {
        val ls: List[Int] = List(1, -1, 2)
        def eval(n: Int): Result[String] = {
          if (n > 0) Passed({ (for (i <- 0 to n) yield i.toString).toStream })
          else Failure("neg")
        }
        def comb(x: String, y: String) = x + y
        combineAll(ls, eval, comb) should be(Passed(Stream("00", "01", "02", "10", "11", "12")))
      }
    }

    describe("parts of a set") {

      it("pSet of 1,2") {
        val bm = parts(Set(1, 2))
        bm.run.get.toSet should be
        Set((Set(1, 2), Set()), (Set(1), Set(2)), (Set(2), Set(1)), (Set(), Set(1, 2)))
      }

      it("pSet of 1,2,3") {
        val bm = parts(Set(1, 2, 3))
        bm.run.get.toSet should be(
          Set((Set(1, 2, 3), Set()),
            (Set(1, 2), Set(3)),
            (Set(1, 3), Set(2)),
            (Set(2, 3), Set(1)),
            (Set(1), Set(2, 3)),
            (Set(2), Set(1, 3)),
            (Set(3), Set(1, 2)),
            (Set(), Set(1, 2, 3))
          ))

      }

    }

  }
}