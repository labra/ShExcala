package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.monads._
import es.weso.monads.Result._
import es.weso.rdf.validator.ValidationResult
import es.weso.shex.ShapeSyntax.Label
import es.weso.rdf.PrefixMap
import util.{
  Failure => TryFailure, 
  Success => TrySuccess
  }

case class ShExResult(value: Result[Typing]) extends ValidationResult[RDFNode,Label,Throwable] {
  
  override def merge(other: ValidationResult[RDFNode,Label,Throwable]): ValidationResult[RDFNode,Label,Throwable] = {
   ???
  }
  
  override def combine(other: ValidationResult[RDFNode,Label,Throwable]): ValidationResult[RDFNode,Label,Throwable] = {
    other match {
      case ShExResult(r) =>  
                 ShExResult(Result.merge(value, r, (t1: Typing,t2: Typing) => t1.combine(t2)))
      case _ => throw new Exception("combine: ShExResult...unexpected: " + other)
    }
  }
  
  
 override def extract: scala.util.Try[Seq[Map[RDFNode,(Seq[Label], Seq[Label])]]] = {
   value.run.map(_.map(typing2Tuple(_))) 
 } 
 
 private def typing2Tuple(t: Typing): Map[RDFNode,(Seq[Label], Seq[Label])] = {
   t.map.mapValues(cnvNodes(_))
 } 
 
 private def cnvNodes(ns: Set[RDFNode]): (Seq[Label],Seq[Label]) = {
   (ns.map(ShapeSyntax.mkLabel(_)).toSeq,Seq())
 }
 

 override def isValid: Boolean = value.isValid

 override def orElse(other: => ValidationResult[RDFNode,Label,Throwable]): ValidationResult[RDFNode,Label,Throwable] = {
   other match {
     case ShExResult(r) => {
       ShExResult(value orelse r)
     }
     case _ => 
       throw new Exception("Unsupported orElse combination with different results: " + other)
   }
 }
  


 override def show(cut: Int)(implicit pm: PrefixMap): String = {
   value.run match {
      case TryFailure(e) => s"Failed: $e"
      case TrySuccess(rs) => {
        if (rs.isEmpty) {
          "<No results>"
        } else {
          val sb = new StringBuilder
          for ((typing, n) <- rs zip (1 to cut)) {
            sb ++= f"Solution ${n}%2d: ${typing.showTyping(pm)}\n"
          }
          sb.toString
        }
      }
   }
 }

}

/*case class Pass(assignment: Map[RDFNode, IRI]) extends ShExResult {

  def assign(node: RDFNode, iri: IRI): ShExResult = {
    Pass(assignment = assignment + (node -> iri))
  }

}

case class NoPass() extends ShExResult

*/