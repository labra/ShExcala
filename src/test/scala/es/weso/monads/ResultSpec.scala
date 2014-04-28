package es.weso.monads

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.monads.Result._
import Stream._

class ResultSpec 
 extends FunSpec 
 with Matchers 
 with Checkers {

 describe("a Result") {
   
   it("Should return a value") {
    val bm : Result[Int] = unit(2)
    bm.run should be (2 #:: Stream.empty)
  }

  it("Should fail") {
    val bm : Result[Int] = failure("example failed")
    intercept[ResultException]{
      bm.run
    }  
  }

  it("Should recover from fail") {
    val bm : Result[Int] = 
      failure("example failed").orelse(unit(2))
    bm.run should be (List(2).toStream)
  }

  it("Should be able to use flatMap") {
    val bm : Result[Int] =
      // for (x <- unit(2)) yield (x + 1)  
      unit(2) flatMap { x => unit(x + 1)}
       
    bm.run should be (List(3).toStream)
  }

  it("Should be able to use for's") {
    val bm : Result[Int] =
      for (x <- unit(2)) yield x + 1  
             
    bm.run should be (List(3).toStream)
  }

  it("Should be able to use for to generate a pair") {
    val bm : Result[(Int,Int)] =
      for ( x <- unit(2)
          ; y <- unit(x + 1)
          ) yield (x,y)  
             
    bm.run should be (List((2,3)).toStream)
  }
 
  it("A sequence with a fail...fails") {
    val u1 : Result[Int] = unit(1)
    val f : Result[Int] = failure("fail")
    val bm = for (x <- u1; y <- f) yield x + y
    bm.isFailure should be(true)
  }

  it("A sequence with a fail and orelse...recovers") {
    val u1 : Result[Int] = unit(1)
    val f : Result[Int] = failure("fail")
    val u2 : Result[Int] = unit(2)
    val bm1 = for (x <- u1; y <- f) yield (x + y)
    val bm2 = bm1 orelse u2
    bm1.isFailure should be(true)
    bm2.isFailure should be(false)
    bm2.run should be (List((2)).toStream)
  }

  it("pSet of 1,2") {
    val bm = parts(Set(1,2))
    bm.run should be (List((Set(1,2),Set()),(Set(1),Set(2)),(Set(2),Set(1)),(Set(),Set(1,2))).toStream)
  }

  it("pSet of 1,2,3") {
    val bm = parts(Set(1,2,3))
    bm.run.toSet should be (
        Set((Set(1,2,3),Set()),
            (Set(1,2),Set(3)),
            (Set(1,3),Set(2)),
            (Set(2,3),Set(1)),
            (Set(1),Set(2,3)),
            (Set(2),Set(1,3)),
            (Set(3),Set(1,2)),
            (Set(),Set(1,2,3))
            ))
  }
 }
}