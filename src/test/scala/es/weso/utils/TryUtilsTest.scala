package es.weso.utils

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import TryUtils._
import util._

class TryUtilsTest extends FunSpec with Matchers with Checkers {

describe("combineAll") {
 type A = String
 type B = (Int,Seq[Int])
 val current = (0,Seq())
 def comb(x: A, current: B): Try[Seq[B]] = {
   (x,current) match {
     case ("0",(m,xs)) => 
       Try(Seq((m+1,xs)))
     case ("1",(m,xs)) => 
       Try(Seq((m+1,1 +: xs)))
     case ("2",(m,xs)) => 
       Try(Seq((m+1,1 +: xs), (m+1,2 +: xs)))
     case ("3",(m,xs)) => 
       fail("Not valid 3")
     case _ => 
       Failure(throw new Exception("Unsupported value"))
   }
 }
 
 it("combines all Seq(1)") {
   val expected: Try[Seq[B]] = {
     Try(Seq((1,Seq(1))))
   }
   TryUtils.combineAll(Seq("1"),current,comb _) should be(expected)
 }
 
  it("combines all Seq(1,2)") {
   val expected: Try[Seq[B]] = {
     Try(Seq((2,Seq(1, 1)), (2,Seq(2, 1))))
   }
   TryUtils.combineAll(Seq("1","2"),current,comb _) should be(expected)
 }

}


}