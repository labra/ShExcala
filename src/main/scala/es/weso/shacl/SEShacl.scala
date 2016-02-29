package es.weso.shacl

import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.rdf._
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import es.weso.shacl.PREFIXES._
import util._
import es.weso.utils.PrefixMapUtils._
import org.slf4j._
import es.weso.rbe.{ 
  Schema => SESchema,
  Shape => SEShape,
  Or => RBEOr,
  _
  }


/**
 * Shacl Abstract Syntax to SESchema 
 */
object SEShacl {
  type SEShaclSchema = SESchema[IRI,RDFNode,Label,ValidationError]
  type Val = (DirectedEdge[IRI],NodeShape[Label,RDFNode,ValidationError])
  type NodeShape_ = NodeShape[Label,RDFNode,ValidationError]

  def shacl2SE(s: SHACLSchema): SEShaclSchema = {
    SESchema(
        m = s.shapes.mapValues(sh => shape2SEShape(sh)),
        ignored = Seq(InverseEdge(sh_scopeNode), InverseEdge(sh_scopeClass)) 
    )
  }
  
  def shape2SEShape(sh: Shape): SEShape[DirectedEdge[IRI],RDFNode,Label,ValidationError] = {
    SEShape(
        rbe = shapeExpr2rbe(sh.shapeExpr),
        // TODO: Consider inverse extras
        extras = sh.extras.map(e => DirectEdge(e)).toSeq,
        closed = sh.isClosed
    )
  }
  
  def shapeExpr2rbe(se: ShapeExpr): Rbe[Val] = {
    se match {
      case e:EmptyShape => Empty
      case tc: TripleConstraint => tripleConstraint2Symbol(tc)
      case GroupShape(_,shapes) => {
       val zero : Rbe[Val] = Empty
       shapes.foldRight(zero)((shape,rest) => And(shapeExpr2rbe(shape),rest))
      }
      case SomeOf(_,shapes) => {
       shapes.map(shapeExpr2rbe).reduce((rbe,rest) => RBEOr(rbe,rest))
      }
      case RepetitionShape(_,shape,card,_,_) => {
        Repeat(shapeExpr2rbe(shape),card.getMin,IntOrUnbounded(card.getMax))
      }
      case _ => 
        throw SEShaclException(s"shapeExpr2rbe: unsupported shape expression: $se")
    }
  }
  
  def tripleConstraint2Symbol(tc: TripleConstraint): Symbol[Val] = {
    val edge = if (tc.inverse) InverseEdge(tc.iri)
    else DirectEdge(tc.iri)
    Symbol(
        a = (edge, valueClass2NodeShape(tc.value)),
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
      case NotShape(label) => RefNot(label) 
      case ConjShapeConstr(labels) => ConjRef(labels) 
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