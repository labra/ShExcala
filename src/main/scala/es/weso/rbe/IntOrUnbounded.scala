package es.weso.rbe


import math.{ 
  max => intMax, 
  min => intMin,
  ceil, floor
  }
import scalaz._
import Scalaz._
import org.scalactic._

case class IntOrUnboundedException(msg:String) 
   extends Exception("IntOrUnbounded: " + msg)


/**
 * A Limit can be either an Int or an Unbounded value
 */
trait IntOrUnbounded {
  def isUnbounded : Boolean
  
  def +(other: => IntOrUnbounded):IntOrUnbounded
  def hasLimit = !isUnbounded
  def getLimit: Int
  
  def max(other: => IntOrUnbounded): IntOrUnbounded = {
    this match { case Unbounded => Unbounded
      case IntLimit(v1) => other match {
        case Unbounded => Unbounded
        case IntLimit(v2) => IntLimit(intMax(v1,v2))
      }
    }
  }
  
  def min(other: => IntOrUnbounded): IntOrUnbounded = {
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
  
  implicit def intOrUnboundedShow: Show[IntOrUnbounded] = {
    Show.shows(_.show) 
  }
  
  override def toString = show
  
  def >(other: IntOrUnbounded): Boolean = {
    (this,other) match {
      case (Unbounded,Unbounded) => false
      case (Unbounded, IntLimit(m)) => true
      case (IntLimit(n),Unbounded) => false
      case (IntLimit(n), IntLimit(m)) => n > m
    }
  }

  def >=(x: Int): Boolean = {
    this match {
      case Unbounded => true
      case IntLimit(m) => m >= x
    }
  }
  
  def <=(x: Int): Boolean = {
    this match {
      case Unbounded => false
      case IntLimit(m) => m <= x
    }
  }


}

case object Unbounded extends IntOrUnbounded {
  def isUnbounded = true
  
  def +(other: => IntOrUnbounded): IntOrUnbounded = 
    Unbounded
    
  def getLimit = 
    throw IntervalsException("Cannot get limit of Unbounded value")
  
}

/**
 * Positive integers 
 */
case class IntLimit(m: Int) extends IntOrUnbounded {
  
  require(m >=0)
  
  def isUnbounded = false
  
  def +(other: =>IntOrUnbounded): IntOrUnbounded = {
    if (other.hasLimit) 
      IntLimit(m + other.getLimit)
    else Unbounded
  }
  
  def getLimit:Int = m
}

object IntOrUnbounded {
  
  implicit def int2LimitInt(x: Int) = IntLimit(x)

  
  private def divIntUp(x: Int, y: Int): Int = {
    (x + y - 1) / y
  }

  private def divIntDown(x: Int, y: Int): Int = {
    x / y
  }
  
  def divIntLimitDown(x: Int, y: IntOrUnbounded): IntOrUnbounded = {
    (x,y) match {
      case (0,IntLimit(0)) => Unbounded // This is to include 0-up cardinalities. Verify if this right
      case (0,_) => _0
      case (n,Unbounded) => _1
      case (n,IntLimit(0)) => Unbounded
      case (n,IntLimit(m)) => divIntDown(x,y.getLimit) 
    }
  }

   def divIntLimitUp(x: Int, y: IntOrUnbounded): IntOrUnbounded = {
     (x,y) match {
       case (0,_) => _0
       case (n,Unbounded) => _1
       case (n,IntLimit(0)) => Unbounded
       case (n,IntLimit(m)) => divIntUp(x,y.getLimit) 
     }
  }
   
  lazy val _0 = IntLimit(0)
  lazy val _1 = IntLimit(1)

}

