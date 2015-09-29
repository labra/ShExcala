package es.weso.rbe

import es.weso.rdfgraph.nodes._
import es.weso.rdf._

/**
 *  RDF as graphs 
 */

case class RDFAsGraph(rdf: RDFReader) extends Graph[IRI,RDFNode] {
  
  def in: RDFNode => Seq[(IRI, RDFNode)] = { n =>
    rdf.triplesWithObject(n).map(t => (t.pred,t.subj)).toSeq
  }
  def out: RDFNode => Seq[(IRI, RDFNode)] = { n => 
    println(s"Checking out of $n")
    val out = rdf.triplesWithSubject(n).map(t => (t.pred,t.obj)).toSeq
    println(s"Out of $n = out")
    out
  }
  def nodes: Seq[RDFNode] = {
    //TODO: extend to predicates and objects?
    rdf.subjects().toSeq
  }
  def triples: Seq[(RDFNode, IRI, RDFNode)] = {
    rdf.rdfTriples.map(t => (t.subj,t.pred,t.obj)).toSeq
  }
}

