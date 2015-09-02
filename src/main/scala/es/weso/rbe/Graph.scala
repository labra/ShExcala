package es.weso.rbe

trait Graph[Edge,Node] {
  def nodes: Seq[Node] 
  
  def out: Node => Seq[(Edge,Node)]
  
  def in: Node => Seq[(Edge,Node)]
  
  def triples: Seq[(Node,Edge,Node)]
  
}

case class GraphMap[Edge,Node](
    m: Map[Node,Seq[(Edge,Node)]]) extends Graph[Edge,Node] {
  def nodes = m.keys.toSeq
  def triples = {
   m.map{case (x,out) => out.map{ case (e,o) => (x,e,o)}}.flatten.toSeq
  }
  
  def out = { n =>
    m.get(n).getOrElse(Seq())
  }
  
  def in = { n => ??? // Implement this function...
    
  }
}

object Graph {
  
}