package es.weso.rbe

import es.weso.collection._
import Interval._

case class SorbeException(msg: String) extends Exception

sealed trait Sorbe[+A] {
  
  def interval[U >: A](bag: Bag[U]): Interval = {
    this match {
      case Empty => Interval(0,Unbounded)
      case Symbol(a,n,m) => {
        val wa = bag.multiplicity(a)
        Interval(divIntLimitUp(wa, m),divIntDown(wa,n))
      }
      case And(v1,v2) => v1.interval(bag) & v2.interval(bag)
      
      case Or(v1,v2) => v1.interval(bag) + v2.interval(bag)
      
      case Star(v) => {
        val ss : Seq[A] = this.symbols
        val wda: Bag[U] = bag.delta(this.symbols) 
        if (wda.size == 0) Interval(0,Unbounded)
        else v.interval(bag)
      }
      
      case Plus(v) => {
        if (bag.delta(this.symbols).size == 0) Interval(0,0)
        else v.interval(bag)
      } 
      
      case _ => throw SorbeException("interval: unsupported expr " + this)  
    }
  }
  
  def contains[U >: A](bag: Bag[U]): Boolean = {
    this.interval(bag).contains(1)
  }
  
  lazy val symbols: Seq[A] = {
    this match {
      case Empty => List()
      case Symbol(a,_,_) => List(a)
      case And(v1,v2) => v1.symbols union v2.symbols
      case Or(v1,v2) => v1.symbols union v2.symbols
      case Star(v) => v.symbols
      case Plus(v) => v.symbols
      case _ => throw SorbeException(s"symbols: unexpected Sorbe expression ${this}") 
    }
  }
} 

case object Empty extends Sorbe[Nothing]
case class Symbol[A](a: A, n: Int, m: IntLimit) extends Sorbe[A]
case class And[A](v1: Sorbe[A], v2: Sorbe[A]) extends Sorbe[A]
case class Or[A](v1: Sorbe[A], v2: Sorbe[A]) extends Sorbe[A]
case class Star[A](v: Sorbe[A]) extends Sorbe[A]
case class Plus[A](v: Sorbe[A]) extends Sorbe[A]

object Sorbe {
  
}
