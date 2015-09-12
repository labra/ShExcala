package es.weso.rbe

import es.weso.utils._
import Checker._

/**
 *  String graphs are mainly used for testing purposes
 */

trait StringGraph extends Graph[String,String] {
}

case class Err(str: String) 

object StringGraph {
  
  implicit def mkErr = Err
  
/*  def cond(x: String, p: String => Boolean,msg: String): Checker[String,Err] = {
    if (p(x)) ok(x)
    else err(Err(msg))
  } */

  lazy val isA: Pred[String,Err] = 
      Pred("isA")(x => 
        cond(x, (x: String) => x == "a","eqA"))

  lazy val integer: Pred[String,Err] = 
      Pred("int")(x => 
        cond(x, (x : String) => x.matches("""\d+"""), "integer"))
        
  lazy val one: Pred[String,Err] = 
      Pred("one")(x => 
        cond(x, (x : String) => x=="1", "== 1"))
      
  lazy val two: Pred[String,Err] = 
      Pred("two")(x => 
        cond(x, (x : String) => x=="2", "== 2"))
          
}