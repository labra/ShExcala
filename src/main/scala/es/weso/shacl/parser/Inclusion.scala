package es.weso.shacl.parser

import es.weso.shacl._
import es.weso.rdf.nodes._


case class Inclusion(
      labels: Option[List[Label]],
      extras: Option[List[IRI]],
      closed: Boolean
)
      
object Inclusion {
  lazy val empty: Inclusion = 
    Inclusion(
        labels = None, 
        extras = None, 
        closed = false
    )

  lazy val closedInclusion: Inclusion = 
    Inclusion.empty.copy(closed = true)
  
  def extrasInclusion(ps: List[IRI]): Inclusion = 
    Inclusion.empty.copy(extras = Some(ps))
    
  def labelsInclusion(labels: List[Label]): Inclusion = 
    Inclusion.empty.copy(labels = Some(labels))

}
  
