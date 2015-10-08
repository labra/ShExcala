package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.collection.immutable.StringOps
import scala.text._
import Document._
import es.weso.rdf._
import arq.iri
import org.slf4j.LoggerFactory
import PREFIXES._
import ValueClass.any
import es.weso.shacl._
import es.weso.utils.Logging
import es.weso.utils.PrefixMapUtils._

case class ShapeDocException(msg:String) 
  extends Exception(s"ShapeDocException: $msg")

case class ShaclDoc(prefixMap: PrefixMap) extends Logging {

  def shaclSchemaDoc(s: SHACLSchema): Document = {
    pmDoc(prefixMap) :/: 
    startActionsDoc(s.startActions) :/:
    startDoc(s.start) :/:
    valueClassesDoc(s.valueClasses) :/: 
    shapesDoc(s.shapes) 
  }
  
  def pmDoc(pm: PrefixMap): Document = {
    pm.pm.foldLeft(empty: Document)(
      (d, x) => d :/: text("prefix ") :: x._1 :: text(":") :: space ::
        text("<") :: text(x._2.str) :: text(">")
    )
  }
  
  def startDoc(s: Option[Label]): Document = {
    if (s.isDefined) {
      text("start = ") :: labelDoc(s.get)
    } else
      text("")
  }
  
  def startActionsDoc(as: Actions): Document = 
    actionsDoc(as)
    
/*  def actionsDoc(as:Actions): Document = {
    mapDocWithSep(as, " ", "\n", iriDoc, x => x)
  } */
  def valueClassesDoc(valueClasses: Map[Label,ValueClassDefinition]): Document = {
    iterDocWithSep(valueClasses, "\n", labelValueClassDefnDoc)
  }

  def labelValueClassDefnDoc(pair: (Label,ValueClassDefinition)): Document = {
    "$" :: labelDoc(pair._1) :: space :: valueClassDefnDoc(pair._2)   
  }
  
  def shapesDoc(shapes: Map[Label,Shape]): Document = {
    iterDocWithSep(shapes, "\n", labelShapeDoc)
  }
  
  def labelShapeDoc(pair: (Label,Shape)): Document = {
    virtualDoc(pair._2.isVirtual) :: 
    labelDoc(pair._1) :: space :: shapeDoc(pair._2)   
  }

  def shapeDoc(shape: Shape): Document = {
    inheritDoc(shape.inherit) ::
    extrasDoc(shape.extras) ::
    closedDoc(shape.isClosed) :: 
    text("{") :: space :: 
    shapeExprDoc(shape.shapeExpr) :: 
    text("}") 
  }
  
  def inheritDoc(inherit: Seq[Label]): Document = {
    if (inherit.isEmpty) text("")
    else text("&") :: seqDocWithSep(inherit, " ", labelDoc) :: space
  }
  
  def extrasDoc(extras: Seq[IRI]): Document = {
    if (extras.isEmpty) text("")
    else text("EXTRA") :: seqDocWithSep(extras," ", iriDoc) :: space
  }
  
  def closedDoc(isClosed: Boolean): Document = {
    if (isClosed) text("CLOSED") :: space
    else text("")
  }
  
  def virtualDoc(isVirtual: Boolean): Document = {
    if (isVirtual) {
      text("VIRTUAL") :: space
    } else empty
  }

  def labelDoc(label: Label): Document = {
    label match {
      case IRILabel(iri) => iriDoc(iri)
      case BNodeLabel(id) => text(id.toString)
    }
  }

  def shapeExprDoc(s: ShapeExpr): Document = {
    s match {
      case t: TripleConstraint => 
        tripleConstraintDoc(t)
      case GroupShape(id,shapes) => 
        idDoc(id) :: "(" :: seqDocWithSep(shapes,",",shapeExprDoc) :: text(")")
      case SomeOf(id,shapes) => 
        idDoc(id) :: "(" :: seqDocWithSep(shapes,"||",shapeExprDoc) :: text(")") 
/*      case OneOf(id,shapes) => 
        idDoc(id) :: "(" :: seqDocWithSep(shapes,"|",shapeExprDoc) :: text(")") */ 
      case RepetitionShape(id,shape,card,annotations,actions) => {
        idDoc(id) :: "(" :: shapeExprDoc(shape) :: ")" :: cardDoc(card) :: 
                    annotationsDoc(annotations) :: actionsDoc(actions)
      }
      case Group2(id,shape1,shape2) => 
        idDoc(id) :: "(" :: shapeExprDoc(shape1) :: "," :: shapeExprDoc(shape2) :: text(")")
      case Or(id,shape1,shape2) => 
        idDoc(id) :: "(" :: shapeExprDoc(shape1) :: "|" :: shapeExprDoc(shape2) :: text(")")
/*      case XOr(id,shape1,shape2) => 
        idDoc(id) :: "(" :: shapeExprDoc(shape1) :: "|" :: shapeExprDoc(shape2) :: text(")") */
      case EmptyShape(id) => idDoc(id) :: text("{}") 
      case IncludeShape(id,label) => {
        idDoc(id) :: text("&") :: labelDoc(label)
      }  
      case x => throw ShapeDocException("Unimplemented string conversion for " + s + " yet")
    }
  }


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

  def actionsDoc(as: Actions): Document = {
    iterDocWithSep(as.toList, "\n", actionDoc)
  }
  
  def annotationsDoc(as: List[Annotation]): Document = {
    iterDocWithSep(as, "\n", annotationDoc)
  }
  
  def annotationDoc(a: Annotation): Document =
    ";" :: space :: iriDoc(a.iri) :: 
    a.value.fold(i => iriDoc(i), l => rdfNodeDoc(l))

  def actionDoc(a: (IRI,String)): Document = {
    "%" :: iriDoc(a._1) :: "{" :: text(a._2) :: text("%}")
  }

   def valueClassDefnDoc(v: ValueClassDefinition): Document = {
    v.defn match {
      case Left((vc,as)) => valueClassDoc(vc) :: space :: actionsDoc(as)
      case Right(External()) => text("EXTERNAL") 
    }
  }

  def valueClassDoc(v: ValueClass): Document = {
    v match {
      case vr: ValueClassRef => "$" :: labelDoc(vr.label)
      case vc: ValueConstr => valueDoc(vc)
      case sc: ShapeConstr => shapeDoc(sc)
    }
  }

  def valueDoc(v: ValueConstr): Document = {
    v match {
      case dt: Datatype => datatypeDoc(dt) 
      case `any` => text(".")
      case vs: ValueSet => valueSetDoc(vs)
      case n: NodeKind => nodeKindDoc(n)
    }
  }
  
  def nodeKindDoc(n: NodeKind): Document = {
    n match {
      case IRIKind(s,facets) => kindDoc("IRI", s, facets)
      case BNodeKind(s,facets) => kindDoc("BNode", s, facets)
      case LiteralKind(facets) => kindDoc("Literal", None, facets)
      case NonLiteralKind(s, facets) => kindDoc("NonLiteral", s, facets)
    }
  }
  
  def kindDoc(str: String, s: Option[ShapeConstr], facets: List[XSFacet]): Document = {
    str :: maybeShapeDoc(s) :: facetsDoc(facets)    
  }
  
  def maybeShapeDoc(s: Option[ShapeConstr]): Document = {
    if (s.isDefined) shapeDoc(s.get) :: space
    else empty
  }
  
  def datatypeDoc(dt: Datatype): Document = {
    iriDoc(dt.v) :: facetsDoc(dt.facets)
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
      case DisjShapeConstr(shapes) => seqDocWithSep(shapes, "OR", labelDoc)
      case SingleShape(shape) => labelDoc(shape)
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
      case ValueStem(stem,exclusions) => 
        iriDoc(stem) :: text("~") :: exclusionsDoc(exclusions)
      case ValueAny(exclusions) => 
        text(".") :: exclusionsDoc(exclusions)
    }
  }

  def exclusionsDoc(exclusions: List[Exclusion]): Document = {
   seqDocWithSep(exclusions," ",exclusionDoc) 
  }
  
  def exclusionDoc(exclusion: Exclusion): Document = {
    val textStem = 
      if (exclusion.isStem) text("~")
      else text("")
    text("-") :: iriDoc(exclusion.iri) :: textStem 
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
  
  def pairDoc[A,B](
      doc1 : A => Document,
      doc2 : B => Document,
      sep:String)(pair:(A,B)): Document = {
    doc1(pair._1) :: sep :: doc2(pair._2)
  }

  def mapDocWithSeps[A,B](m: Map[A,B],
    sepBetweenKeyValue: String,
    sepBetweenPairs: String,
    toDocKey: A => Document,
    toDocValue: B => Document): Document = {
      seqDocWithSep(
          m.toList, 
          sepBetweenPairs, 
          pairDoc(toDocKey,toDocValue,sepBetweenKeyValue)
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
  
  def datatype2String(dt: Datatype)(implicit pm:PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).datatypeDoc(dt))
  }
  
  def label2String(label:Label)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).labelDoc(label))
  } 
}

