package es.weso.rdf.validator
import util._

abstract class ValidationResult[Node,Label] {
  def toTrySeq: Try[Seq[Map[Node,(Set[Label],Set[Label])]]]
  
  def isValid: Boolean
  def isFailure = !isValid
  
  def combine(other: ValidationResult[Node,Label]): ValidationResult[Node,Label]
  def orElse(other: ValidationResult[Node,Label]): ValidationResult[Node,Label]
  
}

object ValidationResult {
  
  def combineAll[A,Node,Label](
      xs: Seq[A], 
      zero: ValidationResult[Node,Label],
      eval: A => ValidationResult[Node,Label]): ValidationResult[Node,Label] = {
    xs.foldRight(zero)((x,r) => eval(x).combine(r)) 
  }
      
  def passSome[A,Node,Label](
      xs: Seq[A],
      zero: ValidationResult[Node,Label],
      eval: A => ValidationResult[Node,Label]): ValidationResult[Node,Label] = {
    xs.foldRight(zero)((x,r) => eval(x) orElse r)
  }

}