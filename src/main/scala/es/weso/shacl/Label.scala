package es.weso.shacl
import scala.util.parsing.input.Positional
import es.weso.rdfgraph.nodes._
import es.weso.rdf._
import es.weso.utils.PrefixMapUtils._

/**
 * Labels
 */
sealed trait Label {
  def getNode(): RDFNode

  def show(implicit pm: PrefixMap): String
}

object Label {
  def mkLabel(str: String): IRILabel = {
    IRILabel(IRI(str))
  }

  def mkLabel(node: RDFNode): Label = {
    node match {
      case b: BNodeId => BNodeLabel(b)
      case i: IRI     => IRILabel(i)
      case _          => throw new Exception("Cannot convert node " + node + " to Label")
    }
  }

}

case class IRILabel(iri: IRI) extends Label {
  override def getNode = iri

  override def show(implicit pm: PrefixMap): String = {
    qualify(iri)
  }

  override def toString: String = {
    iri.toString
  }
}

case class BNodeLabel(bnode: BNodeId) extends Label {
  override def getNode = bnode

  override def show(implicit pm: PrefixMap): String =
    "_:" + bnode.id

  override def toString: String = 
    "_:" + bnode.id
  
}

