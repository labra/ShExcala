package es.weso.shacl
import es.weso.rdf.nodes.IRI

case class Action(name: IRI, contents: String) {
  
  def toTuple: (IRI,String) = {
    (name,contents)
  }
  
}

object Action {
  
   def fromTuple(pair:(IRI,String)): Action = {
    Action(pair._1,pair._2)
  }

}