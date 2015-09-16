package es.weso.utils

import org.scalactic.{
 Or => SOr,
 _ 
}

/**
 * Checker of values of type +A with possible errors of type E
 * This is a facade to abstract on alternative implementations using:
 * - scalaz (Validation)
 * - cats (Validated)
 *  
 */
case class Checker[+A,+E](value: A SOr Every[E]){
  def isOK: Boolean = value.isGood
  
  def fold[V](ok: A => V, err: Seq[E] => V): V = 
    value.fold(ok,(es: Every[E]) => err(es.toSeq))
    
  def map[B](f: A => B): Checker[B,E] = {
    Checker(value.map(f))
  }
} 

object Checker {
    
  def ok[A](x: A): Checker[A,Nothing] = 
    Checker(Good(x))
    
  def err[E](e: E): Checker[Nothing,E] = 
    Checker(Bad(Every(e)))
    
  def errString[E >: Throwable](msg: String): Checker[Nothing,E] = 
    Checker(Bad(Every(throw new Exception(msg))))
    
  def accumErrors[A,E](es:Seq[E], checker: Checker[A,E]): Checker[A,E] = {
    es.foldRight(checker)((e,rest) => accumErr(e,rest)) 
  }
  
  def accumErr[A,E](e:E, checker: Checker[A,E]): Checker[A,E] = {
    Checker(checker.value.fold(
        _ => Bad(Every(e)),
        es => {
          val newEvery : Every[E] = es :+ e 
          Bad(es :+ e)
        }
        )) 
  }
        
  def cond[A,E](x: A, p: A => Boolean, name: String)(implicit ferr: String => E): Checker[A,E] = {
    if (p(x)) ok(x)
    else err(ferr(s"Failed condition $name on $x"))
  }

/*  def cond[A,E >: Throwable](x: A,p: A => Boolean): Checker[A,E] = {
    if (p(x)) ok(x)
      else errString(s"Condition failed on $x")
  } */
  
  def checkSome[A,E](x: A, conds: Seq[A => Checker[A,E]], e: E): Checker[A,E] = {
    val zero : Checker[A,E] = err(e)
    conds.foldLeft(zero)((rest,cond) => if (cond(x).isOK) ok(x) else rest)
  }

  def checkAll[A,E](cs: Seq[Checker[A,E]]): Checker[Seq[A],E] = {
    def op : (Checker[A,E],Checker[Seq[A],E]) => Checker[Seq[A],E] = { (x,rest) => {
      x.fold(g => rest.fold(gs => ok(g +: gs), es => rest), es => accumErrors(es,rest))
    }}
    val zero: Checker[Seq[A],E] = ok(Seq())
    cs.foldRight(zero)(op)
  }
  
   def checkValueAll[A,E](x: A, cs: Seq[A => Checker[A,E]]): Checker[A,E] = {
    def op : (A => Checker[A,E],Checker[A,E]) => Checker[A,E] = { (checker,rest) => {
      checker(x).fold(g => rest.fold(v => ok(v), es => rest), es => accumErrors(es,rest))
    }}
    val zero: Checker[A,E] = ok(x)
    cs.foldRight(zero)(op)
  }


}
  
   
  
