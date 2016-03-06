package es.weso.rbe


import math.{ 
  max => intMax, 
  min => intMin,
  ceil, floor
  }
import scalaz._
import Scalaz._
import org.scalactic._
import IntOrUnbounded._ // Imports implicits to convert Int to IntLimit

case class IntervalsException(msg:String) 
   extends Exception(msg)


/**
 * Definition of intervals (m,n) where m and n can be unbounded
 */
case class Interval(n: IntOrUnbounded, m: IntOrUnbounded) {
  
  /**
   * checks if an interval is empty
   */
  def isEmpty: Boolean = {
    n > m
  }
 
  /**
   * interval addition
   */
  def +(other: Interval): Interval = {
    Interval(
        n = n + other.n, 
        m = m + other.m
        )
  }
  
  /**
   * interval intersection
   */
  def &(other:Interval): Interval = {
    Interval(
        n = n.max(other.n),
        m = m.min(other.m)
        )
  } 
  

  
 private def show: String = {
   if (m.hasLimit && n > m.getLimit) s"<empty(${n};${m})>"
   else "[" + n.show + ";" + m.show + "]"
 }

 implicit def limitShow: Show[IntOrUnbounded] = {
    Show.shows(_.show) 
  }

 override def toString = show

 /**
  * Checks if a value belongs to an interval
  */
 def contains(v: Int) = {
   n <= v && m >= v
 }
 
}

