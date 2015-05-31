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

case class Context(
    rdf: RDFReader, 
    schema: SHACLSchema, 
    typing: Typing, 
    pm: PrefixMap, 
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

  def addTyping(node: RDFNode, label: Label): Result[Context] = {
    this.typing.addShape(node, label) match {
      case Failure(e) => 
        failure("Context: cannot assign label " + label + " to node " + node + "...current typing: " + this.typing + "\nError: " + e.getMessage)
      case 
        Success(newT) => unit(this.copy(typing = newT))
    } 
  } 

  def getIRIs(): List[IRI] = {
    rdf.iris().toList
  }

  def getShapes(): List[Rule] = {
    schema.rules.toList
  }

  def getShape(label: Label): Result[Rule] = {
    schema.findShape(label) match {
      case None => failure("Not found shape with label " + label)
      case Some(sh) => unit(sh)
    }
  }

}

object Context {
  def emptyContext: Context =
    Context(RDFTriples.noTriples, 
        SHACLSchema(id = None, rules = Seq(), start = None), 
        Typing.emptyTyping, 
        pm = PrefixMaps.commonShacl, 
        validateIncoming = false
  )

  def emptyContextWithRev: Context =
    Context(rdf = RDFTriples.noTriples, 
        schema = SHACLSchema(id = None, rules = Seq(), start = None), 
        Typing.emptyTyping, 
        pm = PrefixMaps.commonShacl, 
        validateIncoming = true
    )

}
