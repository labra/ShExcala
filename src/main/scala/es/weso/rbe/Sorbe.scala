package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._

case class SorbeException(msg: String) extends Exception(s"SORBEException: " + msg)

/**
 * This trait defines Single Occurrence Regular Bag Expressions (SORBE)
 * 
 * The algorithm to check that a SORBE contains a bag is PTIME
 * The algorithm has been described in [1] and is based on intervals
 * 
 * [1] Complexity and Expressiveness of ShEx for RDF, 
 *     S. Staworko, I. Boneva, J. Labra, S. Hym, E. Prud'hommeaux, H. Solbrig
 * 
 */
sealed trait Sorbe[+A] {
  
  def interval[U >: A](bag: Bag[U]): Interval = {
    this match {
      case Empty => Interval(0,Unbounded)
      case Symbol(a,n,m) => {
        val wa = bag.multiplicity(a)
        Interval(divIntLimitUp(wa, m),divIntLimitDown(wa,n))
      }
      case And(v1,v2) => v1.interval(bag) & v2.interval(bag)
      
      case Or(v1,v2) => v1.interval(bag) + v2.interval(bag)
      
      case Star(v) => {
        if (noSymbolsInBag(bag)) Interval(0,Unbounded)
        else {
         val ie = v.interval(bag) 
         if (ie.isEmpty) ie
         else Interval(1,Unbounded)
        }
      }
      
      case Plus(v) => {
        if (noSymbolsInBag(bag)) Interval(0,0)
        else {
         val ie = v.interval(bag) 
         if (ie.isEmpty) ie
          else Interval(1,ie.m) 
        }
      } 
      
      case _ => throw SorbeException("interval: unsupported expr " + this)  
    }
  }
  
  def contains[U >: A](bag: Bag[U], open: Boolean): Boolean = {
    if (!open && bagHasExtraSymbols(bag)) false 
    else this.interval(bag).contains(1) 
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
  
  def noSymbolsInBag[U >: A](bag: Bag[U]): Boolean = {
    this.symbols.forall(x => bag.multiplicity(x) == 0)
  }
  
  def bagHasExtraSymbols[U >: A](bag: Bag[U]): Boolean = {
    bag.elems.exists{ case (s,_) => !this.symbols.contains(s) }
  }

} 

case object Empty extends Sorbe[Nothing]
case class Symbol[A](a: A, n: Int, m: IntOrUnbounded) extends Sorbe[A]
case class And[A](v1: Sorbe[A], v2: Sorbe[A]) extends Sorbe[A]
case class Or[A](v1: Sorbe[A], v2: Sorbe[A]) extends Sorbe[A]
case class Star[A](v: Sorbe[A]) extends Sorbe[A]
case class Plus[A](v: Sorbe[A]) extends Sorbe[A]

object Sorbe {
  
}
