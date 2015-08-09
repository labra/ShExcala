package es.weso.monads

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

import scalaz._
import Scalaz._
import org.scalactic._
import TypeCheckedTripleEquals._

class ValidationMonadSpec
    extends FunSpec
    with Matchers
    with Checkers {
  
  type ErrorType = String
  type Comp[A] = ValidationNel[ErrorType,A]
  
  def computeNumber(n:Int): Comp[Int] = {
    Success(n)
  }
  
  def err(msg: String): Comp[Int] = {
   val e: Comp[Int] = ("Error: " + msg).failureNel
   e
  }
  
  def add(m:Int, n:Int): Comp[Int] = for {
    x <- computeNumber(m)
    y <- computeNumber(n)
  } yield (x + y)
  
  describe("Scalaz tests") {
    
    it("should pass some scalaz tests") {
      ((1,"a") |+| (2,"b")) should be((3,"ab"))
    }
    
    it("should add 2 numbers") {
      add(2,3).getOrElse(0) should be(5)
    }
    
    it("should recover from error") {
      err("xxx").getOrElse(0) should be(0)
    }

    it("should show error ") {
      println(err("xxx").show)
      err("xxx").isFailure should be(true)
    }
    
  }
  
}
