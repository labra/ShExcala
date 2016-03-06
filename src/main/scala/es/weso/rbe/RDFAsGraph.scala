package es.weso.rbe

import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.utils.Logging

/**
 *  RDF as graphs 
 */
case class RDFAsGraph(rdf: RDFReader) 
   extends Graph[IRI,RDFNode] with Logging {
  
  def in: RDFNode => Seq[(IRI, RDFNode)] = { n =>
    val in = rdf.triplesWithObject(n).map(t => (t.pred,t.subj)).toSeq
    log.info(s"In of $n = $in")
    in 
  }
  def out: RDFNode => Seq[(IRI, RDFNode)] = { n =>
    log.info(s"Calculating out of $n") 
    val out = rdf.triplesWithSubject(n).map(t => {
     (t.pred,t.obj)
    }).toSeq
    log.info(s"Out of $n = $out")
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

