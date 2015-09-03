package es.weso.rbe

import SESchema._
import es.weso.utils._
import Checker._

/**
 *  String graphs are mainly used for testing purposes
 */

trait StringGraph extends Graph[String,String] {
}

object StringGraph {
    lazy val isA: Pred[String,Throwable] = 
      Pred("isA")(x => 
        Checker.cond(x, (x: String) => x == "a"))

    lazy val integer: Pred[String,Throwable] = 
      Pred("int")( 
          x => Checker.cond(x, (x : String) => x.matches("""\d+""")))
          
    lazy val any: Pred[String,Throwable] = 
      Pred("any")(x => Checker.cond(x, (x : String) => true))
}