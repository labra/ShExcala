package es.weso.shacl
import es.weso.rdf.nodes.IRI

case class Actions(as: Seq[Action]) {
  def isEmpty: Boolean = {
    as.isEmpty
  }
  
  def toList: Seq[(IRI,String)] = as.map(_.toTuple)
}

object Actions {
  def empty: Actions = Actions(Seq())
  
  def fromList(as: Seq[(IRI,String)]) = {
    Actions(as.map(Action.fromTuple(_)))
  }
  
}