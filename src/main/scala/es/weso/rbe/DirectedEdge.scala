package es.weso.rbe

trait DirectedEdge[Edge]

case class DirectEdge[Edge](edge: Edge) extends DirectedEdge[Edge]
case class InverseEdge[Edge](edge: Edge) extends DirectedEdge[Edge]