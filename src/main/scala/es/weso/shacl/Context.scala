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

case class Context(
    rdf: RDFReader, schema: SHACLSchema, typing: Typing, pm: PrefixMap, validateIncoming: Boolean = false) {

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

/*  def containsType(node: RDFNode, maybeType: RDFNode): Boolean = {
    typing.hasType(node).contains(maybeType)
  } 

  def addTyping(node: RDFNode, t: RDFNode): Result[Context] = {
    this.typing.addType(node, t) match {
      case None => failure("Context: cannot assign type " + t + " to iri " + node + "...current typing: " + this.typing)
      case Some(newT) => unit(this.copy(typing = newT))
    } 
  } */

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
/*  def emptyContext: Context =
    Context(RDFTriples.noTriples, SHACLSchema(rules = Seq(), start = None), Typing.emptyTyping, pm = PrefixMaps.commonShex, validateIncoming = false
    )

  def emptyContextWithRev: Context =
    Context(rdf = RDFTriples.noTriples, shEx = ShEx(rules = Seq(), start = None), Typing.emptyTyping, pm = PrefixMaps.commonShex, validateIncoming = true
    )
*/
}
