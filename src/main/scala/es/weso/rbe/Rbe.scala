package es.weso.rbe

import es.weso.collection._
import Interval._
import IntOrUnbounded._
import es.weso.utils.Logging


/**
 * This trait defines Single Occurrence Regular Bag Expressions (Rbe)
 * 
 * == Further info == 
 * 
 * The algorithm to check that a Rbe contains a bag is PTIME
 * The algorithm has been described in [1] and is based on intervals
 * 
 * [1] Complexity and Expressiveness of ShEx for RDF, 
 *     S. Staworko, I. Boneva, J. Labra, S. Hym, E. Prud'hommeaux, H. Solbrig
 * 
 */
sealed trait Rbe[+A] extends Logging {
  
  /**
   * Calculates the interval of a bag from a RBE
   * 
   * The following code follows page 11 of
   * [http://labra.github.io/ShExcala/papers/staworko-icdt15a.pdf]
   */
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
      
      case _ => throw RbeException("interval: unsupported expr " + this)  
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
      case _ => throw RbeException(s"repeatInterval. Unsupported cardinality: ($n,$m)")
    }
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
      case _ => throw RbeException(s"symbols: unexpected Rbe expression ${this}") 
    }
  }
  
  def noSymbolsInBag[U >: A](bag: Bag[U]): Boolean = {
    this.symbols.forall(x => bag.multiplicity(x) == 0)
  }
  
  def bagHasExtraSymbols[U >: A](bag: Bag[U]): Boolean = {
    bag.elems.exists{ case (s,_) => !this.symbols.contains(s) }
  }

  
  private def contains[U >: A](bag: Bag[U], open: Boolean): Boolean = {
    if (!open && bagHasExtraSymbols(bag)) false 
    else this.interval(bag).contains(1) 
  }
  
  /**
   * Checks if a bag is matched by this RBE
   * 
   * @param bag bag to check if matches with current RBE
   * @param open allows extra symbols
   */
  def containsWithRepeats[U >: A](bag: Bag[U], open: Boolean): Boolean = {
    if (containsRepeats) 
      matchDeriv(bag,open)
    else
      contains(bag,open)
  }
  
  /**
   * Checks if a RBE contains repetitions 
   */
  private def containsRepeats: Boolean = {
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
    val d = this.derivBag(bag,open,symbols)
    if (d.nullable) true
    else false
  }
  
  /**
   * Derivative over a bag of symbols
   * @param open allows extra symbols
   * @param controlled limits the extra symbols to those that don't appear in controlled
   */
  def derivBag[U >: A](bag: Bag[U], open: Boolean, controlled: Seq[U]): Rbe[U] = {
    val e: Rbe[U] = this
    def f(x: U, rest: Rbe[U]): Rbe[U] = {
      log.info(s"DerivBag. step: x: $x, rest: $rest")
      val r = rest.deriv(x,open,controlled)
      log.info(s"DerivBag. deriv($x)($rest) = $r")
      r
    }
    bag.toSeq.foldRight(e)(f)
  } 
  
  /**
   * Checks if a rbe is nullable
   */
  private def nullable: Boolean = {
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
  
  
   private def mkAnd[A](r1: => Rbe[A], r2: => Rbe[A]): Rbe[A]= {
    val r = (r1, r2) match {
      case (Empty, e2) => e2
      case (e1, Empty) => e1
      case (f @ Fail(_), _) => f
      case (_, f @ Fail(_)) => f
      case (_, _) => And(r1, r2)
    }
    r
  }
   
  private def mkRange[A](e: Rbe[A], m: Int, n: IntOrUnbounded): Rbe[A] = {
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
   
  private def mkRangeSymbol[A](x: A, m: Int, n: IntOrUnbounded): Rbe[A] = {
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

  private def mkOr[A](r1: => Rbe[A], r2: => Rbe[A]): Rbe[A]= {
    val r = (r1, r2) match {
      case (f @ Fail(_), e2) => e2
      case (e1, f @ Fail(_)) => e1
      case (e1, e2) =>
        if (e1 == e2) e1
        else Or(e1, e2)
    }
    r
  }
  
  def mkRepeat[A](r: => Rbe[A], m: Int, n: IntOrUnbounded): Rbe[A]= {
    Repeat(r,m,n)
  }
  
  def derivSymbol[U >: A](x: U, s: Symbol[U], open: Boolean, controlled: Seq[U]): Rbe[U] = {
    if (x == s.a) {
      if (s.m == IntLimit(0)) 
        Fail(s"Symbol $x doesn't match $s")
      else 
        mkRangeSymbol(s.a, math.max(s.n - 1, 0), s.m minusOne)
    } 
    else if (open && !(controlled contains x)) {
      this
    } else {
      Fail(s"$x doesn't match $s")
    }
  }

  /**
   * derivative of this RBE with regards to a symbol
   * @param x symbol
   * @param open allows extra symbols
   * @param controlled limits the extra symbols to those that don't appear in controlled
   */
  def deriv[U >: A](x: U, open: Boolean, controlled: Seq[U]) : Rbe[U] = {
    this match {
      case f@Fail(_) => f 
      case Empty => 
        if (open && !(controlled contains x)) 
          Empty
        else 
          Fail(s"Unexpected symbol $x") 
      case s@Symbol(a,m,n) => {
       derivSymbol(x,s,open,controlled)
      }  
      case And(e1,e2) => {
        lazy val d1 = e1.deriv(x,open,controlled)
        lazy val d2 = e2.deriv(x,open,controlled)
        mkOr(mkAnd(d1, e2), mkAnd(d2, e1))
      }
      case Or(e1,e2) => {
        lazy val d1 = e1.deriv(x,open,controlled)
        lazy val d2 = e2.deriv(x,open,controlled)
        mkOr(d1, d2)
      }
      case Star(e) => {
        val d = e.deriv(x,open,controlled)
        mkAnd(d, e)
      }
      case Plus(e) => {
        val d = e.deriv(x,open,controlled)
        mkAnd(d, Star(e))
      }
      case Repeat(e,m,n) => {
        lazy val d = e.deriv(x,open,controlled)
        mkAnd(d,mkRange(e, math.max(m - 1, 0), n minusOne))
      }
    }
  }
} 


/**
 * Fail RBE doesn't match
 */
case class Fail(msg:String) extends Rbe[Nothing]

/**
 * Empty RBE
 */
case object Empty extends Rbe[Nothing]

/**
 * Represents a symbol that is repeated between n and m times (m can be unbounded)
 */
case class Symbol[+A](a: A, n: Int, m: IntOrUnbounded) extends Rbe[A]

/**
 * And(v1,v2) represents both v1 and v2 
 */
case class And[A](v1: Rbe[A], v2: Rbe[A]) extends Rbe[A]

/**
 * Or(v1,v2) represents either v1 or v2
 */
case class Or[A](v1: Rbe[A], v2: Rbe[A]) extends Rbe[A]

/**
 * Star(v) represents 0 or more v
 */
case class Star[A](v: Rbe[A]) extends Rbe[A]

/**
 * Plus(v) represents 1 or more appearances of v
 */
case class Plus[A](v: Rbe[A]) extends Rbe[A]

/**
 * Repeat(v,n,m) represents between n and m apperances of v
 */
case class Repeat[A](v: Rbe[A], n: Int, m: IntOrUnbounded) extends Rbe[A]

