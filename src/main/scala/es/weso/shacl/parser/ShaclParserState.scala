package es.weso.shacl.parser

import scala.util.parsing.input._
import es.weso.parser._
import es.weso.rdf._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.shacl.Shacl._

/*
 * State of Shape parser
 * State is composed of:
 * - a list of namespaces,
 * - a list of bNodeLabels 
 * - a list of starts
 * - current baseIRI
 * - a map of shape definitions
 */
case class ShapeParserState(
    val namespaces: PrefixMap,
    val bNodeLabels: BNodeTable,
    val starts: List[Label],
    val baseIRI: IRI,
    val createdShapes: Map[Label,Shape]) {

  def newTable(table: BNodeTable): ShapeParserState =
    copy(bNodeLabels = table)

  def addPrefix(prefix: String, iri: IRI): ShapeParserState =
    copy(namespaces = namespaces.addPrefix(prefix, iri))

  def newBNode: (BNodeId, ShapeParserState) = {
    val (id, t) = bNodeLabels.newBNode
    (id, copy(bNodeLabels = t))
  }

  def newBNode(name: String): (BNodeId, ShapeParserState) = {
    val (id, t) = bNodeLabels.getOrAddBNode(name)
    (id, copy(bNodeLabels = t))
  }

  def newBase(newIRI: IRI) =
    copy(baseIRI = newIRI)

  def addStart(label: Label) =
    copy(starts = starts :+ label)
    
  def newShape(shape: Shape): (Label, ShapeParserState) = {
    val (bnode,s1) = newBNode
    val label = BNodeLabel(bnode)
    (label, s1.copy(createdShapes = createdShapes + (label -> shape)))
  }    

}

object ShapeParserState {

  def initial: ShapeParserState = initial(IRI(""))
  def initial(baseIRI: IRI) =
    ShapeParserState(PrefixMap.empty, BNodeTable.empty, List(), baseIRI, Map())

}