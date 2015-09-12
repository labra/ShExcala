package es.weso.rbe

case class Shape[Edge,Node,Label,Err](
    rbe: Sorbe[(Edge,NodeShape[Label,Node,Err])], 
    extras: Seq[Edge], 
    closed: Boolean
)

object Shape {
  def empty = Shape(rbe = Empty, extras = Seq(), closed = false)
}
