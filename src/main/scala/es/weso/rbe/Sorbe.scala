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
      case Fail(_) => Interval(1,0)
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
      
      // Adding Repetitions on expressions breaks the single-occurrence bag expression
      case Repeat(v,n,m) => {
        v.repeatInterval(n,m,bag)
      } 
      
      case _ => throw SorbeException("interval: unsupported expr " + this)  
    }
  }
  
  // TODO: The following recursive code is not optimized. 
  // It could be done tailrec although it may be better to find a mathematical formula
  def repeatInterval[U >: A](n: Int, m: IntOrUnbounded, bag: Bag[U]): Interval = {
    (n,m) match {
      case (0, IntLimit(0)) => Interval(0,Unbounded)
      case (0, IntLimit(m)) => (Or(this,Empty)).interval(bag) & this.repeatInterval(0,IntLimit(m-1),bag)
      case (n, IntLimit(m)) if n > 0 && m >= n => this.interval(bag) & this.repeatInterval(n-1,IntLimit(m-1),bag)
      case (0, Unbounded) => Star(this).interval(bag)
      case (n, Unbounded) if n > 0 => this.interval(bag) & this.repeatInterval(n-1,Unbounded,bag)
      case _ => throw SorbeException(s"repeatInterval. Unsupported cardinality: ($n,$m)")
    }
  }
  
  def contains[U >: A](bag: Bag[U], open: Boolean): Boolean = {
    if (!open && bagHasExtraSymbols(bag)) false 
    else this.interval(bag).contains(1) 
  }
  
  def containsWithRepeats[U >: A](bag: Bag[U], open: Boolean): Boolean = {
    if (containsRepeats) 
      matchDeriv(bag,open)
    else
      contains(bag,open)
  }
  
  def containsRepeats: Boolean = {
    this match {
      case Fail(_) => false
      case Empty => false
      case Symbol(_,_,_) => false
      case And(e1,e2) => e1.containsRepeats || e2.containsRepeats
      case Or(e1,e2) => e1.containsRepeats || e2.containsRepeats
      case Star(e) => e.containsRepeats
      case Plus(e) => e.containsRepeats
      case Repeat(e,m,n) => true
    }
  }

  /**
   * Match a RBE with a bag using the derivatives algorithm
   * open: allows extra symbols to match
   */
  def matchDeriv[U >: A](bag: Bag[U], open: Boolean): Boolean = {
     val d = this.derivBag(bag,open)
     if (d.nullable) true
     else false
  }
  
  def derivBag[U >: A](bag: Bag[U], open: Boolean): Sorbe[U] = {
    val e: Sorbe[U] = this
    def f(x: U, rest: Sorbe[U]): Sorbe[U] = {
      println(s"Step: x: $x, rest: $rest, ")
      val r = rest.deriv(x,open)
      println(s"Deriv($x)($rest) = $r")
      r
    }
    bag.toSeq.foldRight(e)(f)
  } 
  
  lazy val symbols: Seq[A] = {
    this match {
      case Fail(_) => List()
      case Empty => List()
      case Symbol(a,_,_) => List(a)
      case And(v1,v2) => v1.symbols union v2.symbols
      case Or(v1,v2) => v1.symbols union v2.symbols
      case Star(v) => v.symbols
      case Plus(v) => v.symbols
      case Repeat(v,_,_) => v.symbols
      case _ => throw SorbeException(s"symbols: unexpected Sorbe expression ${this}") 
    }
  }
  
  def noSymbolsInBag[U >: A](bag: Bag[U]): Boolean = {
    this.symbols.forall(x => bag.multiplicity(x) == 0)
  }
  
  def bagHasExtraSymbols[U >: A](bag: Bag[U]): Boolean = {
    bag.elems.exists{ case (s,_) => !this.symbols.contains(s) }
  }
  
  def nullable: Boolean = {
    this match {
      case Fail(_) => false
      case Empty => true
      case Symbol(_,0,_) => true
      case Symbol(_,_,_) => false
      case And(e1,e2) => e1.nullable && e2.nullable
      case Or(e1,e2) => e1.nullable || e2.nullable
      case Star(e) => true
      case Plus(e) => false
      case Repeat(e,m,n) => m == 0 || e.nullable
    }
  }
  
  
   def mkAnd[A](r1: => Sorbe[A], r2: => Sorbe[A]): Sorbe[A]= {
    val r = (r1, r2) match {
      case (Empty, e2) => e2
      case (e1, Empty) => e1
      case (f @ Fail(_), _) => f
      case (_, f @ Fail(_)) => f
      case (_, _) => And(r1, r2)
    }
    r
  }
   
  def mkRange[A](e: Sorbe[A], m: Int, n: IntOrUnbounded): Sorbe[A] = {
    if (m < 0) Fail("Range with negative lower bound = " + m)
    else if (m > n) Fail("Range with lower bound " + m + " bigger than upper bound " + n)
    else {
      (m, n, e) match {
        case (0, IntLimit(0), _) => Empty
        case (1, IntLimit(1), e) => e
        case (_, _, f @ Fail(_)) => f
        case (_, _, e @ Empty) => e
        case (m, n, e) => Repeat(e, m, n)
      }
    }
  }
   
  def mkRangeSymbol[A](x: A, m: Int, n: IntOrUnbounded): Sorbe[A] = {
    if (m < 0) Fail("Range with negative lower bound = " + m)
    else if (m > n) Fail("Range with lower bound " + m + " bigger than upper bound " + n)
    else {
      (m, n) match {
//        case (0, IntLimit(0)) => Empty
//        case (1, IntLimit(1)) => Empty
        case (m, n) => Symbol(x, m, n)
      }
    }
  }

  def mkOr[A](r1: => Sorbe[A], r2: => Sorbe[A]): Sorbe[A]= {
    val r = (r1, r2) match {
      case (f @ Fail(_), e2) => e2
      case (e1, f @ Fail(_)) => e1
      case (e1, e2) =>
        if (e1 == e2) e1
        else Or(e1, e2)
    }
    r
  }
  
  def derivSymbol[U >: A](x: U, s: Symbol[U], open: Boolean): Sorbe[U] = {
    if (x == s.a) {
      if (s.m == IntLimit(0)) 
        Fail(s"Symbol $x doesn't match $s")
      else 
        mkRangeSymbol(s.a, math.max(s.n - 1, 0), s.m minusOne)
    } 
    else if (open) {
      this
    } else {
      Fail(s"$x doesn't match $s")
    }
  }

  def deriv[U >: A](x: U, open: Boolean) : Sorbe[U] = {
    this match {
      case f@Fail(_) => f 
      case Empty => 
        if (open) Empty
        else Fail(s"Unexpected symbol $x") 
      case s@Symbol(a,m,n) => {
       derivSymbol(x,s,open)
      }  
      case And(e1,e2) => {
        lazy val d1 = e1.deriv(x,open)
        lazy val d2 = e2.deriv(x,open)
        mkOr(mkAnd(d1, e2), mkAnd(d2, e1))
      }
      case Or(e1,e2) => {
        lazy val d1 = e1.deriv(x,open)
        lazy val d2 = e2.deriv(x,open)
        mkOr(d1, d2)
      }
      case Star(e) => {
        val d = e.deriv(x,open)
        mkAnd(d, e)
      }
      case Plus(e) => {
        val d = e.deriv(x,open)
        mkAnd(d, Star(e))
      }
      case Repeat(e,m,n) => {
        lazy val d = e.deriv(x,open)
        mkAnd(d,mkRange(e, math.max(m - 1, 0), n minusOne))
      }
    }
  }
} 

case class Fail(msg:String) extends Sorbe[Nothing]
case object Empty extends Sorbe[Nothing]
case class Symbol[+A](a: A, n: Int, m: IntOrUnbounded) extends Sorbe[A]
case class And[A](v1: Sorbe[A], v2: Sorbe[A]) extends Sorbe[A]
case class Or[A](v1: Sorbe[A], v2: Sorbe[A]) extends Sorbe[A]
case class Star[A](v: Sorbe[A]) extends Sorbe[A]
case class Plus[A](v: Sorbe[A]) extends Sorbe[A]
case class Repeat[A](v: Sorbe[A], n: Int, m: IntOrUnbounded) extends Sorbe[A]

object Sorbe {

}
