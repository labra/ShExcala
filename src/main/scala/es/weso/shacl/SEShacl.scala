package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shacl.PREFIXES._
import util._
import es.weso.utils.PrefixMapUtils._
import org.slf4j._
import es.weso.rbe.SESchema.{
  Schema => SESchema,
  Shape => SEShape,
  _
  }
import es.weso.rbe._
import Shacl._


/**
 * Shacl Abstract Syntax to SESchema 
 */
object SEShacl {
  type SEShaclSchema = SESchema[IRI,RDFNode,Label]
  type Val = (IRI,NodeShape[Label,RDFNode])
  type NodeShape_ = NodeShape[Label,RDFNode]

  def shacl2SE(s: SHACLSchema): SEShaclSchema = {
    SESchema(m = s.shapes.mapValues(sh => shape2SEShape(sh)))
  }
  
  def shape2SEShape(sh: Shape): SEShape[IRI,RDFNode,Label] = {
    SEShape(
        rbe = shapeExpr2rbe(sh.shapeExpr),
        extras = sh.extras.toSeq,
        closed = sh.isClosed
    )
  }
  
  def shapeExpr2rbe(se: ShapeExpr): Sorbe[Val] = {
    se match {
      case e:EmptyShape => Empty
      case tc: TripleConstraint => tripleConstraint2Symbol(tc)
      case _ => 
        throw SEShaclException(s"shapeExpr2rbe: unsupported shape expression: $se")
    }
  }
  
  def tripleConstraint2Symbol(tc: TripleConstraint): Symbol[Val] = {
    Symbol(
        a = (tc.iri, valueClass2NodeShape(tc.value)),
        n = tc.card.getMin,
        m = IntOrUnbounded(tc.card.getMax)
    )
  }
  
  def valueClass2NodeShape(v: ValueClass): NodeShape[Label,RDFNode] = {
    v match {
      case vc: ValueConstr => valueConstr2NodeShape(vc) 
      case _ => throw SEShaclException(s"valueClass2NodeShape: Unsupported value class " + v)
    }
  }
  
  def valueConstr2NodeShape(vc: ValueConstr): NodeShape_ = {
    vc match {
      case nk: NodeKind => nodeKind2NodeShape(nk)
      case _ => throw SEShaclException(s"valueConstr2NodeShape: Unsupported value constr" + vc)
    }
  }
  def nodeKind2NodeShape(nk: NodeKind): NodeShape_ = {
    nk match {
      case IRIKind(_,_) => 
        Pred(name = "IRIKind", 
             pred = (n: RDFNode) => n.isIRI
            )
      case _ => throw SEShaclException(s"nodeKind2NodeShape: Unsupported node kind" + nk)
    }
  }
}

case class SEShaclException(msg: String) extends Exception("SEShaclException: " + msg)