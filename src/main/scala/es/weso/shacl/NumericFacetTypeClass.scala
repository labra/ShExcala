package es.weso.shacl
import es.weso.utils.Checker
import es.weso.utils.Checker._

trait NumericFacetTypeClass[T] {
    def fractionDigits(x:T):Int
    
    def totalDigits(x:T):Int
    
    def compareVals(x:T, y:T): Int
    
    def checkFractionDigits(x:T,n:Int): Checker[T,ValidationError] = {
      if (fractionDigits(x) == n) ok(x)
      else err(MsgError(s"Fraction digits = $n failed for $x"))
    }
    def checkTotalDigits(x:T,n:Int): Checker[T,ValidationError] = {
      if (totalDigits(x) == n) ok(x)
      else err(MsgError(s"Total digits = $n failed for $x"))
    }
    def minInclusive(x:T,y:T): Checker[T,ValidationError] = {
      if (compareVals(x,y) >= 0) ok(x)
      else err(MsgError(s"minInclusive condition failed between $x and $y"))
    }
    def minExclusive(x:T,y:T): Checker[T,ValidationError] = {
      if (compareVals(x,y) > 0) ok(x)
      else err(MsgError(s"minExclusive condition failed between $x and $y"))
    }
    def maxInclusive(x:T,y:T): Checker[T,ValidationError] = {
      if (compareVals(x,y) <= 0) ok(x)
      else err(MsgError(s"maxInclusive condition failed between $x and $y"))
    }
    def maxExclusive(x:T,y:T): Checker[T,ValidationError] = {
      if (compareVals(x,y) < 0) ok(x)
      else err(MsgError(s"maxExclusive condition failed between $x and $y"))
    }
  }
  
object NumericFacetTypeClass {
  
  implicit object NumericFacetDouble extends NumericFacetTypeClass[Double] {
    def fractionDigits(x:Double): Int = {
      val str = x.toString
      if (str contains '.')
        str.reverse.takeWhile(_ != '.').length
      else
        0      
    }
    def totalDigits(x:Double): Int = {
      x.toString.length
    }
    def compareVals(x:Double,y:Double): Int = {
      x compare y
    }
  }
  
  implicit object NumericFacetDecimal extends NumericFacetTypeClass[BigDecimal] {
    def fractionDigits(x:BigDecimal): Int = {
      val str = x.toString
      if (str contains '.')
        str.reverse.takeWhile(_ != '.').length
      else
        0      
    }
    def totalDigits(x:BigDecimal): Int = {
      x.toString.length
    }
    def compareVals(x:BigDecimal,y:BigDecimal): Int = {
      x compare y
    }
  }
  
  implicit object NumericFacetInteger extends NumericFacetTypeClass[Integer] {
    def fractionDigits(x:Integer): Int = 0
    
    def totalDigits(x:Integer): Int = {
      x.toString.length
    }
    def compareVals(x:Integer,y:Integer): Int = {
      x.toInt compare y.toInt
    }
  }
  
  implicit object NumericFacetString extends NumericFacetTypeClass[String] {
    def fractionDigits(x:String): Int = {
      if (x contains '.')
        x.reverse.takeWhile(_ != '.').length
      else
        0
    }
    
    def totalDigits(x:String): Int = {
      x.length
    }
    def compareVals(x:String,y:String): Int = {
      x.toDouble compare y.toDouble
    }
  }
}
  