package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.collection.immutable.StringOps
import scala.text._
import Document._
import es.weso.rdf._
import arq.iri
import org.slf4j.LoggerFactory
import es.weso.shacl.PREFIXES._
import es.weso.shacl.Shacl._
import es.weso.utils.Logging
import es.weso.utils.PrefixMapUtils._

case class ShapeDocException(msg:String) extends Exception

case class ShaclDoc(prefixMap: PrefixMap) extends Logging {

  def shaclSchemaDoc(s: SHACLSchema): Document = {
    pmDoc(prefixMap) :/: 
    shapesDoc(s.shapes) // TODO: start
  }

  def pmDoc(pm: PrefixMap): Document = {
    pm.pm.foldLeft(empty: Document)(
      (d, x) => d :/: text("prefix ") :: x._1 :: text(":") :: space ::
        text("<") :: text(x._2.str) :: text(">")
    )
  }

  def shapesDoc(shapes: Map[Label,Shape]): Document = {
    iterDocWithSep(shapes, "\n", labelShapeDoc)
  }
  
  def labelShapeDoc(pair: (Label,Shape)): Document = {
    labelDoc(pair._1) :: shapeDoc(pair._2)
  }

  def shapeDoc(shape: Shape): Document = {
      shapeExprDoc(shape.shapeExpr) 
      // TODO: Closed and inherit
  }

  def labelDoc(label: Label): Document = {
    label match {
      case IRILabel(iri) => iriDoc(iri)
      case BNodeLabel(id) => text(id.toString)
    }
  }

  def shapeExprDoc(s: ShapeExpr): Document = {
    s match {
//      case t: TripleConstraint => tripleConstraintDoc(t)
      case t: TripleConstraint => tripleConstraintDoc(t)
      case GroupShape(id,shapes) => 
        "(" :: seqDocWithSep(shapes,",",shapeExprDoc) :: text(")")
      case SomeOf(id,shapes) => 
        log.info("Unimplemented id generation yet")
        "(" :: seqDocWithSep(shapes,"||",shapeExprDoc) :: text(")") 
      case OneOf(id,shapes) => 
        log.info("Unimplemented id generation yet")
        "(" :: seqDocWithSep(shapes,"|",shapeExprDoc) :: text(")") 
      case RepetitionShape(id,shape,card) => {
        log.info("Unimplemented id generation yet")
        "(" :: shapeExprDoc(shape) :: ")" :: cardDoc(card)
      }
      case Group2(id,shape1,shape2) => 
        "(" :: shapeExprDoc(shape1) :: "," :: shapeExprDoc(shape2) :: text(")")
      case Or(id,shape1,shape2) => 
        "(" :: shapeExprDoc(shape1) :: "||" :: shapeExprDoc(shape2) :: text(")")
      case XOr(id,shape1,shape2) => 
        "(" :: shapeExprDoc(shape1) :: "|" :: shapeExprDoc(shape2) :: text(")")
      case EmptyShape(id) => text("{}") 
        
      case x => throw ShapeDocException("Unimplemented string conversion for " + s + " yet")
    }
  }

/*  def tripleConstraintDoc(t: TripleConstraint): Document = {
    log.info("Unimplemented id generation yet")
    iriDoc(t.iri) :: space :: valueClassDoc(t.value) 
  } */

  def tripleConstraintDoc(t: TripleConstraint): Document = {
    idDoc(t.id) ::
    inverseDoc(t.inverse) :: 
    negatedDoc(t.negated) :: 
    iriDoc(t.iri) :: space :: 
    valueClassDoc(t.value) :: 
    cardDoc(t.card) ::
    annotationsDoc(t.annotations)
  }
  
  def idDoc(id: Option[Label]): Document = 
    id.fold(text(""))(
        label => "$" :: labelDoc(label) :: space
    )
  
  def inverseDoc(i: Boolean): Document = 
    if (i) text("^")
    else text("")

  def negatedDoc(i: Boolean): Document = 
    if (i) text("!")
    else text("")

  def actionsDoc(as: Map[Label,String]): Document = {
    iterDocWithSep(as, "\n", actionDoc)
  }
  
  def annotationsDoc(as: List[Annotation]): Document = {
    iterDocWithSep(as, "\n", annotationDoc)
  }
  
  def annotationDoc(a: Annotation): Document =
    ";" :: space :: iriDoc(a.iri) :: 
    a.value.fold(i => iriDoc(i), l => rdfNodeDoc(l))

  def actionDoc(a: (Label,String)): Document = {
    "%" :: labelDoc(a._1) :: "{" :: text(a._2) :: text("}")
  }

  def valueClassDoc(v: ValueClass): Document = {
    v match {
      case vc: ValueConstr => valueDoc(vc)
      case sc: ShapeConstr => shapeDoc(sc)
    }
  }

  def valueDoc(v: ValueConstr): Document = {
    v match {
      case dt: LiteralDatatype => literalDatatypeDoc(dt) 
      case `any` => text(".")
      case vs: ValueSet => valueSetDoc(vs)
      case n: NodeKind => text(n.token)
    }
  }
  
  def literalDatatypeDoc(dt: LiteralDatatype): Document = {
    rdfNodeDoc(dt.v) :: facetsDoc(dt.facets)
  }

  def valueSetDoc(vs: ValueSet): Document = {
    "(" :: nest(3, seqDocWithSep(vs.s, " ", valueObjectDoc)) :: text(")")
  }
  
  def facetsDoc(facets: Seq[XSFacet]): Document = {
    seqDocWithSep(facets, " ", facetDoc)
  }

  def facetDoc(facet: XSFacet): Document = {
    facet match {
      case Pattern(regex) => "PATTERN " :: text(regex.toString)
      case MinInclusive(n) => "MININCLUSIVE " :: text(n.toString)
      case MaxInclusive(n) => "MAXINCLUSIVE " :: text(n.toString)
      case MinExclusive(n) => "MINEXCLUSIVE " :: text(n.toString)
      case MaxExclusive(n) => "MAXEXCLUSIVE " :: text(n.toString)
      case Length(n) => "LENGTH " :: text(n.toString)
      case MinLength(n) => "MINLENGTH " :: text(n.toString)
      case MaxLength(n) => "MAXLENGTH " :: text(n.toString)
      case TotalDigits(n) => "TOTALDIGITS " :: text(n.toString)
      case FractionDigits(n) => "FRACTIONDIGITS " :: text(n.toString)
    }
  }

  def shapeDoc(s: ShapeConstr): Document = {
    s match {
      case DisjShapeConstr(shapes) => setDocWithSep(shapes, "OR", labelDoc)
      case _ => throw new Exception(s"shapeDoc: unsupported $s")
    }
  }

  def cardDoc(v: Cardinality): Document = {
    v match {
      case RangeCardinality(m, n) => "{" :: m.toString :: "," :: n.toString :: text("}")
      case UnboundedCardinalityFrom(m) =>
        m match {
          case 0 => text("*")
          case 1 => text("+")
          case _ => "{" :: m.toString :: "," :: "unbounded" :: text("}")
        }
    }
  }

  def rdfNodeDoc(n: RDFNode): Document = {
    text(showRDFNode(n)(prefixMap))
  }

  def valueObjectDoc(v: ValueObject): Document = {
    v match {
      case ValueIRI(iri) => iriDoc(iri)
      case ValueLiteral(l) => rdfNodeDoc(l)
      case ValueLang(lang) => text("@") :: text(lang.lang)
      case ValueStem(stem,exclusions) => throw new Exception("Unsupported valuestem")
      case ValueAny(exclusions) => throw new Exception("Unsupported valueAny")
    }
  }

  def iriDoc(i: IRI): Document = {
    text(qualify(i)(prefixMap))
  }

  def pairDoc(d1: Document, d2: Document): Document =
    "(" :: d1 :: "," :: d2 :: ")" :: empty

  def space: Document = text(" ")

  def seqDocWithSep[A](s: Seq[A],
    sep: String,
    toDoc: A => Document): Document = {
    if (s.isEmpty) empty
    else
      s.tail.foldLeft(toDoc(s.head))(
        (d: Document, x: A) => d :: sep :/: toDoc(x)
      )
  }

  def setDocWithSep[A](s: Set[A],
    sep: String,
    toDoc: A => Document): Document = {
    if (s.isEmpty) empty
    else
      s.tail.foldLeft(toDoc(s.head))(
        (d: Document, x: A) => d :: sep :/: toDoc(x)
      )
  }

  def iterDocWithSep[A](m: Iterable[A],
    sep: String,
    toDoc: A => Document): Document = {
    if (m.isEmpty) empty
    else
      m.tail.foldLeft(toDoc(m.head))(
        (d: Document, x: A) => d :: sep :/: toDoc(x)
      )
  }

}

object ShaclDoc {

  /**
   * Generic function for pretty printing
   */
  def prettyPrint(d: Document): String = {
    val writer = new java.io.StringWriter
    d.format(1, writer)
    writer.toString
  }

  def schema2String(s: SHACLSchema)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).shaclSchemaDoc(s))
  }

  def shape2String(shape: ShapeExpr)(implicit pm: PrefixMap): String =
    prettyPrint(ShaclDoc(pm).shapeExprDoc(shape))

  def valueSet2String(vs: ValueSet)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).valueSetDoc(vs))
  }

  def nodeKind2String(nk: NodeKind): String = nk.token
  
  def literalDatatype2String(dt: LiteralDatatype)(implicit pm:PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).literalDatatypeDoc(dt))
  }
  
  def label2String(label:Label)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).labelDoc(label))
  } 
}

