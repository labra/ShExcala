package es.weso.shacl

import es.weso.rdf._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.monads.Result
import es.weso.monads.Result._
import es.weso.rdfgraph.nodes.IRI
import org.slf4j._
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.rdf.PrefixMap
import util._

case class ContextException(msg: String) extends RuntimeException

case class Context(
    rdf: RDFReader, 
    schema: SHACLSchema, 
    typing: Typing, 
    pm: PrefixMap, 
    pending: List[RDFTriple],
    validateIncoming: Boolean = false) {

  val log = LoggerFactory.getLogger("Context")
  

  def triplesWithSubject(n: RDFNode): Set[RDFTriple] = {
    rdf.triplesWithSubject(n)
  }

  def triplesWithObject(n: RDFNode): Set[RDFTriple] = {
    rdf.triplesWithObject(n)
  }

  def triplesAround(n: RDFNode): Set[RDFTriple] = {
    rdf.triplesWithSubject(n) ++
      (if (validateIncoming) rdf.triplesWithObject(n)
      else Set())
  }

  def containsType(node: RDFNode, label: Label): Boolean = {
    typing.containsType(node,label)
  } 

  def containsNegType(node: RDFNode, label: Label): Boolean = {
    typing.containsNegType(node,label)
  } 
  
  def addTyping(node: RDFNode, label: Label): Try[Context] = {
    typing.addShape(node, label) match {
      case Failure(e) => 
        Failure(ContextException("Context: cannot assign label " + label + " to node " + node + "\nCurrent typing: " + this.typing + "\nError: " + e.getMessage))
      case 
        Success(newT) => Success(this.copy(typing = newT))
    } 
  } 

  def getIRIs(): List[IRI] = {
    rdf.iris().toList
  }

  def getShape(label: Label): Option[Shape] = {
    schema.findShape(label) 
  }

}

object Context {
  def emptyContext: Context =
    Context(RDFTriples.noTriples, 
        SHACLSchema(id = None, shapes = Map(), start = None), 
        Typing.emptyTyping, 
        pm = PrefixMaps.commonShacl, 
        pending = List(),
        validateIncoming = false
  )
    

}
