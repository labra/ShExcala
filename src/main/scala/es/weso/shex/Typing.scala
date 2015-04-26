package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._

import es.weso.rdf._
import scala.util.parsing.input.Positional

case class Typing(map: Map[RDFNode, Set[RDFNode]]) {

  def addType(key: RDFNode, value: RDFNode): Option[Typing] = {
    if (map contains key) {
      Some(Typing(map.updated(key, map(key) + value)))
    } else {
      Some(Typing(map + ((key, Set(value)))))
    }
  }

  def rmType(key: RDFNode, value: RDFNode): Option[Typing] = {
    if ((map contains key) && (map(key) contains value)) {
      val newSet = map(key) - value
      if (newSet.isEmpty) {
        Some(Typing(map - key))
      } else {
        Some(Typing(map.updated(key, newSet)))
      }
    } else None
  }

  def hasType(n: RDFNode): Set[RDFNode] = {
    if (map contains n) map(n)
    else Set()
  }

  def combine(other: Typing): Typing = {
    Typing(map ++ other.map)
  }

  def hasTypes(n: RDFNode, nodes: Set[RDFNode]): Boolean = {
    hasType(n) == nodes
  }

  def showTyping(implicit pm: PrefixMap): String = {
    val sb = new StringBuilder
    for (is <- map) {
      sb ++= (showNode(is._1) + " -> " + showNodes(is._2) + "\n")
    }
    sb.toString
  }

  // TODO: Refactor to put these 2 definitions in RDF
  private def showIRI(iri: IRI)(implicit pm: PrefixMap): String = {
    "<" + iri.str + ">"
  }

  private def showNode(node: RDFNode)(implicit pm: PrefixMap): String = {
    if (node.isIRI) showIRI(node.toIRI)
    else node.toString
  }

  private def showNodes(nodes: Set[RDFNode])(implicit pm: PrefixMap): String = {
    val sb = new StringBuilder
    sb ++= "("
    for (n <- nodes) {
      sb ++= (showNode(n) + " ")
    }
    sb ++= ")"
    sb.toString
  }

}

object Typing {

  def emptyTyping: Typing = Typing(Map[RDFNode, Set[RDFNode]]())

}