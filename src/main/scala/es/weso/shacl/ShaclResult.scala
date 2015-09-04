package es.weso.shacl

import util._
import es.weso.typing._
import es.weso.rdfgraph.nodes._
import es.weso.rdf.validator._
import es.weso.shacl.Shacl._ 

case class ShaclResult(value: Try[Seq[PosNegTyping[RDFNode,Label]]]) extends ValidationResult[RDFNode,Label] {

 override def combine(other: ValidationResult[RDFNode,Label]): ShaclResult = ???
 
 override def toTrySeq = value.map(_.map(toTuple))
 
 override def toTuple(t: PosNegTyping[RDFNode,Label]): Map[RDFNode,(Set[Label],Set[Label])] = {
   t.asMap.mapValues(tr => (tr.posLabels,tr.negLabels))
 }
 
 override def getNodes: Seq[RDFNode] = ???
 
 override def getPositiveLabels(node: RDFNode): Seq[Label] = ???
 
 override def isValid: Boolean = ???

 override def orElse(other: ValidationResult[RDFNode,Label]): ValidationResult[RDFNode,Label] = ???
}

object ShaclResult {
  def empty = ShaclResult(Success(Seq()))
}