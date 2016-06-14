package es.weso.shex
import es.weso.validating._
import es.weso.rdf.nodes.{DecimalLiteral, DoubleLiteral, IntegerLiteral, RDFNode}
import Checked._
import Constraint._

trait NumericFacetTypeClass[T] {

    def toRDFNode(v: T): RDFNode

    def fractionDigits(x:T):Int
    
    def totalDigits(x:T):Int
    
    def compareVals(x:T, y:T): Int
    
    def checkFractionDigits(x:T,n:Int): Checked[T,ConstraintReason,ConstraintError[T]] = {
      if (fractionDigits(x) == n) okSingle(x,s"Matched fractionDigits($x,$n)")
      else errString(s"Fraction digits = $n failed for $x")
    }
    def checkTotalDigits(x:T,n:Int): Checked[T,ConstraintReason,ConstraintError[T]] = {
      if (totalDigits(x) == n) okSingle(x,s"Matched totalDigits($x,$n)")
      else errString(s"Total digits = $n failed for $x")
    }
    def minInclusive(x:T,y:T): Checked[T,ConstraintReason,ConstraintError[T]] = {
      if (compareVals(x,y) >= 0) okSingle(x,s"Matched minInclusive($x,$y)")
      else errString(s"minInclusive failed between $x and $y")
    }
    def minExclusive(x:T,y:T): Checked[T,ConstraintReason,ConstraintError[T]] = {
      if (compareVals(x,y) > 0) okSingle(x,s"Matched minExclusive($x,$y)")
      else errString(s"minExclusive failed between $x and $y")
    }
    def maxInclusive(x:T,y:T): Checked[T,ConstraintReason,ConstraintError[T]] = {
      if (compareVals(x,y) <= 0) okSingle(x,s"Matched maxInclusive($x,$y)")
      else errString(s"maxInclusive failed between $x and $y")
    }
    def maxExclusive(x:T,y:T): Checked[T,ConstraintReason,ConstraintError[T]] = {
      if (compareVals(x,y) < 0) okSingle(x,s"Matched maxExclusive($x,$y)")
      else errString(s"maxExclusive failed between $x and $y")
    }
  }
  
object NumericFacetTypeClass {
  
  implicit object NumericFacetDouble extends NumericFacetTypeClass[Double] {

    def toRDFNode(t: Double): RDFNode = DoubleLiteral(t)

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

    def toRDFNode(t: BigDecimal): RDFNode = DecimalLiteral(t)

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
    def toRDFNode(t: Integer): RDFNode = IntegerLiteral(t)

    def fractionDigits(x:Integer): Int = 0
    
    def totalDigits(x:Integer): Int = {
      x.toString.length
    }
    def compareVals(x:Integer,y:Integer): Int = {
      x.toInt compare y.toInt
    }
  }
  
  implicit object NumericFacetString extends NumericFacetTypeClass[String] {
    def toRDFNode(t: String): RDFNode = ???

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
  