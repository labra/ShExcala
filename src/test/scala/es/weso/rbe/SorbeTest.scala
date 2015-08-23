package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._

class SorbeTest extends FunSpec with Matchers {
  
  describe("Intervals calculation") {
    equalInterval(Empty,Bag.toBag(List("a","b","a")), Interval(0,Unbounded))
    equalInterval(Symbol("a",1,2),Bag.toBag(List("a","b","a")), Interval(1,2))
    equalInterval(Symbol("a",2,4),Bag.toBag(List("a","b","a")), Interval(1,1))
    equalInterval(Symbol("a",2,3),Bag.toBag(List("a","b","a")), Interval(1,1))
    equalInterval(Symbol("a",2,400),Bag.toBag(List("a","b","a")), Interval(1,1))
    equalInterval(Symbol("a",1,1),Bag.toBag(List("a","b","a")), Interval(2,2))
    equalInterval(Symbol("a",2,2),Bag.toBag(List("a","b","a","a","a","a","a")), Interval(3,3))
    equalInterval(Symbol("b",2,2),Bag.toBag(List("a","b","a","a","a","a","a")), Interval(1,0))
    equalInterval(Symbol("b",1,1),Bag.toBag(List("a","b","a","a","a","a","a")), Interval(1,1))
    equalInterval(Symbol("c",1,1),Bag.toBag(List("a","b","a","a","a","a","a")), Interval(0,0))
    equalInterval(And(Symbol("a",1,2),Symbol("b",1,1)),
        Bag.toBag(List("a","b","a")), Interval(1,1))
    equalInterval(And(Symbol("a",1,2),Symbol("b",2,2)),
        Bag.toBag(List("a","b","a")), Interval(1,0))
  }
  
  describe("Containment calculation") {
    containsBag(Empty,Bag.toBag(List("a","b","a")))
    containsBag(Symbol("a",1,2),Bag.toBag(List("a","b","a")))
    containsBag(Symbol("a",2,4),Bag.toBag(List("a","b","a")))
    containsBag(Symbol("a",2,3),Bag.toBag(List("a","b","a")))
    containsBag(Symbol("a",2,400),Bag.toBag(List("a","b","a")))
    notContainsBag(Symbol("a",1,1),Bag.toBag(List("a","b","a")))
    notContainsBag(Symbol("a",2,2),Bag.toBag(List("a","b","a","a","a","a","a")))
    notContainsBag(Symbol("b",2,2),Bag.toBag(List("a","b","a","a","a","a","a")))
    containsBag(Symbol("b",1,1),Bag.toBag(List("a","b","a","a","a","a","a")))
    notContainsBag(Symbol("c",1,1),Bag.toBag(List("a","b","a","a","a","a","a")))
    containsBag(And(Symbol("a",1,2),Symbol("b",1,1)),
        Bag.toBag(List("a","b","a")))
  }
  
  def equalInterval[A](rbe: Sorbe[A], bag: Bag[A], expected: Interval) = {
    it(s"Interval of ${bag} with ${rbe} should be ${expected}") {
      rbe.interval(bag) should be(expected)
    }
  }
  
  def containsBag[A](rbe: Sorbe[A], bag: Bag[A]) = {
    it(s"${rbe} should contain ${bag}") {
      rbe.contains(bag) should be(true)
    }
  }
  
  def notContainsBag[A](rbe: Sorbe[A], bag: Bag[A]) = {
    it(s"${rbe} should not contain ${bag}") {
      rbe.contains(bag) should be(false)
    }
  }
}