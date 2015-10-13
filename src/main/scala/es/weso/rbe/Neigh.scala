package es.weso.rbe

/**
 * Represents a neighbour of a node in a graph
 * It can be a direct arc or an inverse arc
 */
sealed trait Neigh[Edge,Node] {
  def directedEdge: DirectedEdge[Edge]
  def node: Node
  def isDirect: Boolean
  
  def mkTriple(node: Node): (Node,Edge,Node)
}

case class Direct[Edge,Node](edge: Edge, node: Node) extends Neigh[Edge,Node] {
  
 override def directedEdge: DirectedEdge[Edge] = DirectEdge(edge)

 override def isDirect = true
 
 override def toString: String = {
   "-(" + edge + ")-> " + node
 }
 
 override def mkTriple(n: Node): (Node,Edge,Node) = {
   (n,edge,node)
 }
}
case class Inverse[Edge,Node](edge: Edge, node: Node) extends Neigh[Edge,Node] {

 override def directedEdge: DirectedEdge[Edge] = InverseEdge(edge)
  
 override def isDirect = false
  
 override def toString: String = {
   "<-(" + edge + ")- " + node
 }

 override def mkTriple(n: Node): (Node,Edge,Node) = {
   (node,edge,n)
 }
}
