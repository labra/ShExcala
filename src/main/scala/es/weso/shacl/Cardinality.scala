package es.weso.shacl
import scala.util.parsing.input.Positional

sealed trait Cardinality extends Positional {
  def minusOne: Cardinality
  def getMin: Int
  def getMax: Option[Int]
}

object Cardinality {
  lazy val star = UnboundedCardinalityFrom(0)
  lazy val plus = UnboundedCardinalityFrom(1)
  lazy val optional = RangeCardinality(0, 1)
  lazy val defaultCardinality = RangeCardinality(1, 1)
  def defaultMaxCardinality(m: Int) = RangeCardinality(1, m)

}

case class RangeCardinality(m: Int, n: Int) extends Cardinality {
  require(m >= 0)
  require(m <= n)

  def minusOne =
    this match {
      case RangeCardinality(0, 0)                   => this
      case RangeCardinality(0, n) if n > 0          => RangeCardinality(0, n - 1)
      case RangeCardinality(m, n) if m > 0 && n > 0 => RangeCardinality(m - 1, n - 1)
      case _                                        => throw new Exception("minusOne: Unexpected cardinality " + this)
    }

  def getMin = m

  def getMax = Some(n)
}

/**
 * UnboundedCardinality represents ranges (m,unbounded)
 */
case class UnboundedCardinalityFrom(m: Int) extends Cardinality {
  require(m >= 0)

  def minusOne =
    this match {
      case UnboundedCardinalityFrom(0)          => UnboundedCardinalityFrom(0)
      case UnboundedCardinalityFrom(n) if n > 0 => UnboundedCardinalityFrom(m - 1)
      case _                                    => throw new Exception("minusOne: Unexpected cardinality " + this)
    }

  def getMin = m

  def getMax = None
}
