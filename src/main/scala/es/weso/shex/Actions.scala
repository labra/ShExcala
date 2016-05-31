package es.weso.shex
import es.weso.rdf.nodes.IRI

case class Actions(as: Seq[Action]) {
  def isEmpty: Boolean = {
    as.isEmpty
  }
  
  def toList: Seq[(IRI,String)] = as.map(_.toTuple)
  
  def ++(other: Actions): Actions = {
    Actions(as ++ other.as)
  }
}

object Actions {
  def empty: Actions = Actions(Seq())
  
  def fromList(as: Seq[(IRI,String)]) = {
    Actions(as.map(Action.fromTuple(_)))
  }
  
}