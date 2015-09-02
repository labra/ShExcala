package es.weso.shacl
import org.scalactic._

case class Checker[+E,+A](value: A Or Every[E]){
  def isOK: Boolean = value.isGood
} 

object Checker {

  def errString[E >: Throwable](msg: String): Checker[E,Nothing] = 
    Checker(Bad(Every(throw new Exception(msg))))
    
  def ok[A](x: A): Checker[Nothing,A] = Checker(Good(x))
  def err[E](e: E): Checker[E,Nothing] = Checker(Bad(Every(e)))
  def accumErr[E,A](e:E, checker: Checker[E,A]): Checker[E,A] = {
    Checker(checker.value.fold(
        _ => Bad(Every(e)),
        es => {
          val newEvery : Every[E] = es :+ e 
          Bad(es :+ e)
        }
        )) 
  }
}