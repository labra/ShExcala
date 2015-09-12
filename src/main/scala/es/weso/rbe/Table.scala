package es.weso.rbe

case class Table[Edge,Node,Label,Err](
    constraints: Map[ConstraintRef,NodeShape[Label,Node,Err]],
    edges: Map[Edge,Set[ConstraintRef]],
    elems: Int) {
  def addEdge(
      e: Edge, 
      n: ConstraintRef): Map[Edge,Set[ConstraintRef]] = {
    edges.updated(e, 
        edges.get(e).getOrElse(Set()) + n)
  }
}

object Table {
  def empty[Edge,Node,Label,Err]: Table[Edge,Node,Label,Err] =
    Table(Map(),Map(),0)
}    
