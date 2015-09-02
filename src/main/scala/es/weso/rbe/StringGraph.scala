package es.weso.rbe

import SESchema._

/**
 *  String graphs are mainly used for testing purposes
 */

trait StringGraph extends Graph[String,String] {
}

object StringGraph {
    lazy val isA: Pred[String] = Pred("isA", _ == "a")
    lazy val integer: Pred[String] = Pred("int", _.matches("""\d+"""))
    lazy val any: Pred[String] = Pred("any", _ => true)
}