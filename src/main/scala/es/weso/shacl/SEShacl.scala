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
  type SEShaclSchema = SESchema[IRI,RDFNode,Label,ValidationError]
  type Val = (IRI,NodeShape[Label,RDFNode,ValidationError])
  type NodeShape_ = NodeShape[Label,RDFNode,ValidationError]

  def shacl2SE(s: SHACLSchema): SEShaclSchema = {
    SESchema(m = s.shapes.mapValues(sh => shape2SEShape(sh)))
  }
  
  def shape2SEShape(sh: Shape): SEShape[IRI,RDFNode,Label,ValidationError] = {
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
  
  def valueClass2NodeShape(v: ValueClass): NodeShape[Label,RDFNode,ValidationError] = {
    v match {
      case vc: ValueConstr => valueConstr2NodeShape(vc)
      case sc: ShapeConstr => shapeConstr2NodeShape(sc)
      case _ => throw SEShaclException(s"valueClass2NodeShape: Unsupported value class " + v)
    }
  }
  
  def shapeConstr2NodeShape(sc: ShapeConstr): NodeShape[Label,RDFNode,ValidationError] = {
    sc match {
      case SingleShape(label) => Ref(label) 
      case _ => throw SEShaclException(s"shapeConstrNodeShape: Unsupported value class " + sc)
    }
  }
  
  def valueConstr2NodeShape(vc: ValueConstr): NodeShape_ = {
    Pred(name = s"ValueConstr $vc")( 
         pred = (n: RDFNode) => vc.check(n)
        )
  }
  
}

case class SEShaclException(msg: String) extends Exception("SEShaclException: " + msg)