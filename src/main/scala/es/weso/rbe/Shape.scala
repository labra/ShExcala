package es.weso.rbe

/**
 * A shape contains a regular bag expression, a closed modifier and a list of extras
 * 
 * @param rbe regular bag expression
 * @param extras list of extra edges that are allowed
 * @param closed the shape is closed
 */
case class Shape[Edge,Node,Label,Err](
    rbe: Rbe[(Edge,NodeShape[Label,Node,Err])], 
    extras: Seq[Edge], 
    closed: Boolean
)

object Shape {
  def empty = Shape(rbe = Empty, extras = Seq(), closed = false)
}
