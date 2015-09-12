package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._

class SorbeTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  
  describe("Intervals calculation") {
    
    val emptyBag : Bag[String] = Bag.toBag(List())
    
    equalInterval(Empty,Bag.
        toBag(List("a","b","a")), Interval(0,Unbounded))
        
    equalInterval(Symbol("a",0,1),
        Bag.toBag(List("b")), Interval(0,Unbounded))
        
    equalInterval(Symbol("a",0,1),
        Bag.toBag(List("a","b")), Interval(1,Unbounded))
        
    equalInterval(Symbol("a",0,1),
        Bag.toBag(List("a","b","a")), Interval(2,Unbounded))
        
    equalInterval(Symbol("a",1,2),
        Bag.toBag(List("a","b","a")), Interval(1,2))
        
    equalInterval(Symbol("a",2,4),
        Bag.toBag(List("a","b","a")), Interval(1,1))
        
    equalInterval(Symbol("a",2,3),
        Bag.toBag(List("a","b","a")), Interval(1,1))
        
    equalInterval(Symbol("a",2,400),
        Bag.toBag(List("a","b","a")), Interval(1,1))
        
    equalInterval(Symbol("a",1,1),
        Bag.toBag(List("a","b","a")), Interval(2,2))
        
    equalInterval(Symbol("a",2,2),
        Bag.toBag(List("a","b","a","a","a","a","a")), Interval(3,3))
        
    equalInterval(Symbol("b",2,2),
        Bag.toBag(List("a","b","a","a","a","a","a")), Interval(1,0))
        
    equalInterval(Symbol("b",1,1),
        Bag.toBag(List("a","b","a","a","a","a","a")), Interval(1,1))
        
    equalInterval(Symbol("c",1,1),
        Bag.toBag(List("a","b","a","a","a","a","a")), Interval(0,0))
        
    equalInterval(And(Symbol("a",1,2),Symbol("b",1,1)),
        Bag.toBag(List("a","b","a")), Interval(1,1))
        
    equalInterval(And(Symbol("a",1,2),Symbol("b",2,2)),
        Bag.toBag(List("a","b","a")), Interval(1,0))
        
    equalInterval(Symbol("a",0,0),Bag.toBag(List("a","b")), 
        Interval(Unbounded,Unbounded))
    
    equalInterval(Symbol("a",0,0),
        Bag.toBag(List("b")), Interval(0,Unbounded))
    
    equalInterval(Symbol("b",0,0),
        Bag.toBag(List("b")), Interval(Unbounded,Unbounded))
    
    equalInterval(Plus(Symbol("a",1,1)),
        Bag.toBag(List("a","a","a")), Interval(1,3))
        
    equalInterval(Plus(Symbol("a",1,1)),
        Bag.toBag(List("b","b","c")), Interval(0,0))
        
    equalInterval(Star(Symbol("a",1,1)),
        Bag.toBag(List("a","a","a")), Interval(1,Unbounded))
        
    equalInterval(Star(Symbol("a",1,1)),
        Bag.toBag(List("b","b","c")), Interval(0,Unbounded))
        
    equalInterval(And(Symbol("a",1,1),Symbol("b",1,1)),
        Bag.toBag(List("a")), Interval(1,0))

    equalInterval(And(Symbol("a",1,1),Symbol("b",1,1)),
        Bag.toBag(List("b")), Interval(1,0))
        
    equalInterval(And(Symbol("a",1,1),Symbol("b",1,1)),
        emptyBag, Interval(0,0))

    equalInterval(And(Symbol("a",1,1),And(Symbol("b",1,1),Empty)),
        emptyBag, Interval(0,0))
    
    equalInterval(And(Symbol("a",1,1),Empty),
        emptyBag, Interval(0,0))
        
    equalInterval(Or(Symbol("a",1,1),Or(Symbol("b",1,1),Empty)),
        emptyBag, Interval(0,0))
    
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
    notContainsBag(And(Symbol("a",1,2),Symbol("c",1,1)),
        Bag.toBag(List("a","b","a")))
    containsBag(Or(Symbol("a",1,2),Symbol("c",1,1)),
        Bag.toBag(List("a","b","a")))
    notContainsBag(Or(Symbol("a",3,5),Symbol("c",1,1)),
        Bag.toBag(List("a","b","a")))
    notContainsBag(Or(Symbol("a",0,1),Symbol("c",1,1)),
        Bag.toBag(List("a","b","a")))
    notContainsBag(Symbol("a",0,0),Bag.toBag(List("a","b","a")))
    notContainsBag(Symbol("a",0,0),Bag.toBag(List("a","b")))
    
    containsBag(Symbol("a",0,0),Bag.toBag(List("b")))
    
    notContainsBag(Symbol("b",0,0),Bag.toBag(List("b")))

    
    containsBag(Or(Symbol("a",0,0),Symbol("b",1,1)),Bag.toBag(List("b")))
    containsBag(Or(Symbol("a",0,0),Symbol("b",1,1)),Bag.toBag(List("c")))
    containsBag(Or(Symbol("a",0,0),Symbol("b",1,1)),Bag.toBag(List("b","c")))
    notContainsBag(Or(Symbol("a",0,0),Symbol("b",1,1)),Bag.toBag(List("a","b")))
    containsBag(Or(Symbol("a",1,1),Symbol("b",1,1)),Bag.toBag(List("b","c")))
    notContainsBag(Or(Symbol("a",1,1),Symbol("b",1,1)),Bag.toBag(List("a","b","c")))
    containsBag(Or(Symbol("a",2,2),Symbol("b",1,1)),Bag.toBag(List("a","a","c")))
    notContainsBag(Or(Symbol("a",2,2),Symbol("b",1,1)),Bag.toBag(List("a","a","c")),false)
  }
  
  def equalInterval[A](rbe: Sorbe[A], bag: Bag[A], expected: Interval) = {
    it(s"Interval of ${bag} with ${rbe} should be ${expected}") {
      rbe.interval(bag) should be(expected)
    }
  }
  
  def containsBag[A](rbe: Sorbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should contain ${bag}. Open: $open") {
      rbe.contains(bag,open) should be(true)
    }
  }
  
  def notContainsBag[A](rbe: Sorbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should not contain ${bag}, Open: $open") {
      rbe.contains(bag,open) should be(false)
    }
  }
 
}