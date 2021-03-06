package es.weso.shex

import util._
import es.weso.typing._
import es.weso.rdf.nodes._
import es.weso.rdf.validator._
import es.weso.rdf.PrefixMap
import es.weso.utils.TryUtils._
import es.weso.utils.SeqUtils._

case class ShExResult(value: Try[Seq[PosNegTyping[RDFNode, Label]]])
    extends ValidationResult[RDFNode, Label, Throwable] {

  override def combine(other: ValidationResult[RDFNode, Label, Throwable]): ShExResult = {
    other match {
      case sr: ShExResult => (value, sr.value) match {
        case (Failure(e), _)              => ShExResult(Failure(e))
        case (Success(_), f @ Failure(e)) => ShExResult(f)
        case (Success(ts1), Success(ts2)) => ShExResult(Success(ts1 ++ ts2))
      }
    }
  }
  

  override def merge(other: ValidationResult[RDFNode, Label, Throwable]): ShExResult = {
    other match {
      case sr: ShExResult => (value, sr.value) match {
        case (Failure(e), _)              => ShExResult(Failure(e))
        case (Success(_), f @ Failure(e)) => ShExResult(f)
        case (Success(ts1), Success(ts2)) => {
          val results = 
            mergeSeqs(ts1,ts2,
                (t1:PosNegTyping[RDFNode,Label],t2: PosNegTyping[RDFNode,Label]) => t1.combine(t2),
                (t: PosNegTyping[RDFNode,Label]) => Success(t))
          val combs = filterSuccess(results)
          ShExResult(combs)
        } 
      }
    }
  }

  override def extract: Try[Seq[Map[RDFNode,(Seq[Label],Seq[Label])]]] = 
    value.map(_.map(toTuple))

  def toTuple(t: PosNegTyping[RDFNode, Label]): Map[RDFNode, (Seq[Label], Seq[Label])] = {
    t.asMap.mapValues(tr => (tr.posLabels.toSeq, tr.negLabels.toSeq))
  }

  override def isFailure: Boolean = value.isFailure || value.get.isEmpty
  override def isValid: Boolean = !isFailure

  override def orElse(other: => ValidationResult[RDFNode, Label, Throwable]): ValidationResult[RDFNode, Label, Throwable] = {
    other match {
      case sr: ShExResult =>
        value match {
          case Success(ts1) => sr.value match {
            case Success(ts2) => ShExResult(Success(ts1 ++ ts2))
            case Failure(_)   => ShExResult(Success(ts1))
          }
          case Failure(_) => other

        }

      case _ => throw new Exception(s"Unsupported orElse of this: $this with value $other")
    }
  }
  
  def show(cut: Int)(implicit pm: PrefixMap): String = {
    value match {
      case Failure(e) => s"Failed: $e"
      case Success(rs) => {
        if (rs.isEmpty) {
          "<No results>"
        } else {
          val sb = new StringBuilder
          sb ++= s"${rs.size} Results\n"
          for ((r, n) <- rs zip (1 to cut)) {
            sb ++= f"Solution ${n}%2d: $r\n"
          }
          sb.toString
        }
      }
    }
  } 

}

object ShExResult {
  def empty = ShExResult(Success(Seq()))
  
  def fail(msg: String) = ShExResult(Failure(new Exception(msg)))
}