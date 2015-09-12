package es.weso.rdf.validator
import util._
import es.weso.rdf.PrefixMap

abstract class ValidationResult[Node,Label,Err] {
  def extract: Try[Seq[Map[Node,(Seq[Label],Seq[Label])]]]
  
  def isValid: Boolean
  def isFailure = !isValid
  
  def combine(other: ValidationResult[Node,Label,Err]): ValidationResult[Node,Label,Err]
  def merge(other: ValidationResult[Node,Label,Err]): ValidationResult[Node,Label,Err]
  def orElse(other: => ValidationResult[Node,Label,Err]): ValidationResult[Node,Label,Err]
  
  def show(cut: Int)(implicit pm: PrefixMap): String 

}

object ValidationResult {
  
  def combineAll[A,Node,Label,Err](
      xs: Seq[A], 
      zero: ValidationResult[Node,Label,Err],
      eval: A => ValidationResult[Node,Label,Err]): ValidationResult[Node,Label,Err] = {
    xs.foldRight(zero)((x,r) => eval(x).combine(r)) 
  }
  
  def mergeAll[A,Node,Label,Err](
      xs: Seq[A], 
      zero: ValidationResult[Node,Label,Err],
      eval: A => ValidationResult[Node,Label,Err]): ValidationResult[Node,Label,Err] = {
    xs.foldRight(zero)((x,r) => eval(x).merge(r)) 
  }
  
  def passSome[A,Node,Label,Err](
      xs: Seq[A],
      zero: ValidationResult[Node,Label,Err],
      eval: A => ValidationResult[Node,Label,Err]): ValidationResult[Node,Label,Err] = {
    xs.foldRight(zero)((x,r) => eval(x) orElse r)
  }

}