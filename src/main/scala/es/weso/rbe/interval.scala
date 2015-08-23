package es.weso.rbe


import math.{ 
  max => intMax, 
  min => intMin,
  ceil, floor
  }
import scalaz._
import Scalaz._
import org.scalactic._

case class IntervalsException(msg:String) 
   extends Exception(msg)


/**
 * A Limit can be either an Int or an Unbounded value
 */
trait Limit {
  def isUnbounded : Boolean
  
  def +(other: => Limit):Limit
  def hasLimit = !isUnbounded
  def getLimit: Int
  
  def max(other: => Limit): Limit = {
    this match { case Unbounded => Unbounded
      case IntLimit(v1) => other match {
        case Unbounded => Unbounded
        case IntLimit(v2) => IntLimit(intMax(v1,v2))
      }
    }
  }
  
  def min(other: => Limit): Limit = {
    this match {
      case Unbounded => other
      case IntLimit(v1) => other match {
        case Unbounded => Unbounded
        case IntLimit(v2) => IntLimit(intMin(v1,v2))
      }
    }
  }
  
  def show: String = {
    this match {
      case Unbounded => "-"
      case IntLimit(v) => v.toString
    }
  }
  
  implicit def limitShow: Show[Limit] = {
    Show.shows(_.show) 
  }
  
  override def toString = show
  
  def >=(x: Int): Boolean = {
    this match {
      case Unbounded => true
      case IntLimit(m) => m >= x
    }
  }

}

case object Unbounded extends Limit {
  def isUnbounded = true
  
  def +(other: => Limit): Limit = 
    Unbounded
    
  def getLimit = 
    throw IntervalsException("Cannot get limit of Unbounded value")
  
}

/**
 * Positive integers 
 */
case class IntLimit(m: Int) extends Limit {
  
  require(m >=0)
  
  def isUnbounded = false
  
  def +(other: =>Limit): Limit = {
    if (other.hasLimit) 
      IntLimit(m + other.getLimit)
    else Unbounded
  }
  
  def getLimit:Int = m
}


case class Interval(n: Int, m: Limit) {
 
  def +(other: Interval): Interval = {
    Interval(
        n = n + other.n, 
        m = m + other.m
        )
  }
  
  def &(other:Interval): Interval = {
    Interval(
        n = intMax(n,other.n),
        m = m.min(other.m)
        )
  } 
  
  
 def show: String = {
   if (m.hasLimit && n > m.getLimit) s"<empty(${n};${m})>"
   else "[" + n.toString + ";" + m.show + "]"
 }

 implicit def limitShow: Show[Limit] = {
    Show.shows(_.show) 
  }

 override def toString = show
 
 def contains(v: Int) = {
   v >= n && m >= v
 }
 
}

object Interval {
  // def apply(n: Int, m:Int): Interval = Interval(n,IntLimit(m))
  
  def divIntUp(x: Int, y: Int): Int = {
    (x + y - 1) / y
  }

  def divIntDown(x: Int, y: Int): Int = {
    x / y
  }
  
  def divIntLimitDown(x: Int, y: IntLimit): Int = {
    if (y.hasLimit) {
      divIntDown(x,y.getLimit)
    } else {
      if (x == 0) 0 else 1
    }
  }

   def divIntLimitUp(x: Int, y: IntLimit): Int = {
    if (y.hasLimit) {
      divIntUp(x,y.getLimit)
    } else {
      if (x == 0) 0 else 1
    }
  }

  implicit def int2LimitInt(x: Int) = IntLimit(x)
}
