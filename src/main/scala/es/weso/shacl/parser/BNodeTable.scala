package es.weso.shacl.parser

import scala.collection.immutable.Map

import es.weso.rdf.nodes.BNodeId

case class BNodeTable(
    bNodeName: Map[BNodeId, Option[String]],
    nameBNode: Map[String, BNodeId],
    nodes: Int = 0
) {

  def newBNode: (BNodeId, BNodeTable) =
    (
      BNodeId("b" + nodes),
      BNodeTable(bNodeName, nameBNode, nodes + 1)
    )

  def getOrAddBNode(idName: String): (BNodeId, BNodeTable) = {
    nameBNode.get(idName) match {
      case None => {
        val id = BNodeId(idName)
        (id, BNodeTable(
          bNodeName + (id -> Some(idName)),
          nameBNode + (idName -> id),
          nodes + 1
        ))
      }
      case Some(bNodeId) => (bNodeId, this)
    }
  }

  def getBNodeId(name: String): Option[BNodeId] = {
    nameBNode.get(name)
  }

  def getBNodeName(id: BNodeId): Option[String] = {
    bNodeName.get(id).getOrElse(None)
  }

  /*  def clear() : Unit = {
    nodes = 0
    bNodeName.clear
    nameBNode.clear
  } */

  override def toString: String = {
    "Nodes: " + nodes + ", bNodeName: " + bNodeName.toString + ", nameBNode: " + nameBNode
  }

}

object BNodeTable {
  def empty: BNodeTable = BNodeTable(Map[BNodeId, Option[String]](), Map[String, BNodeId](), 0)
}