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
    rulesDoc(s.rules) // TODO: start
  }

  def pmDoc(pm: PrefixMap): Document = {
    pm.pm.foldLeft(empty: Document)(
      (d, x) => d :/: text("prefix ") :: x._1 :: text(":") :: space ::
        text("<") :: text(x._2.str) :: text(">")
    )
  }

  def rulesDoc(rules: Seq[Rule]): Document = {
    seqDocWithSep(rules, "\n", ruleDoc)
  }

  def ruleDoc(rule: Rule): Document = {
    labelDoc(rule.label) ::
      shapeDefinitionDoc(rule.shapeDefinition) ::
      extensionConditionsDoc(rule.extensionCondition)
  }

  def shapeDefinitionDoc(shapeDefn: ShapeDefinition): Document = {
    shapeDefn match {
      case OpenShape(s, inclSet) =>
        space :: "{" :: space ::
          nest(3,
            group(shapeExprDoc(s))) ::
            space ::
            text("}")
      case ClosedShape(s) =>
        space :: "[" :: space ::
          nest(3, group(shapeExprDoc(s))) ::
          space ::
          text("]")
    }
  }

  def labelDoc(label: Label): Document = {
    label match {
      case IRILabel(iri) => iriDoc(iri)
      case BNodeLabel(id) => text(id.toString)
    }
  }

  def shapeExprDoc(s: ShapeExpr): Document = {
    s match {
      case t: TripleConstraint => tripleConstraintDoc(t)
      case t: InverseTripleConstraint => inverseTripleConstraintDoc(t)
      case t: TripleConstraintCard => tripleConstraintCardDoc(t)
      case GroupShape(id,shapes) => 
        "(" :: seqDocWithSep(shapes,",",shapeExprDoc) :: text(")")
      case SomeOfShape(id,shapes) => 
        log.info("Unimplemented id generation yet")
        "(" :: seqDocWithSep(shapes,"||",shapeExprDoc) :: text(")") 
      case OneOfShape(id,shapes) => 
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
      case EmptyShape => text("{}") 
        
      case x => throw ShapeDocException("Unimplemented string conversion for " + s + " yet")
    }
  }

  def tripleConstraintDoc(t: TripleConstraint): Document = {
    log.info("Unimplemented id generation yet")
    iriDoc(t.iri) :: space :: valueClassDoc(t.value) 
  }

  def inverseTripleConstraintDoc(t: InverseTripleConstraint): Document = {
    log.info("Unimplemented id generation yet")
    "^" :: iriDoc(t.iri) :: space :: shapeDoc(t.shape) 
  }

  def tripleConstraintCardDoc(t: TripleConstraintCard): Document = {
    log.info("Unimplemented id generation yet")
    iriDoc(t.iri) :: space :: valueClassDoc(t.value) :: cardDoc(t.card)
  }

  def inverseTripleConstraintCardDoc(t: InverseTripleConstraintCard): Document = {
    log.info("Unimplemented id generation yet")
    "^" :: iriDoc(t.iri) :: space :: shapeDoc(t.shape) :: cardDoc(t.card)
  }

  def extensionConditionsDoc(es: Seq[ExtensionCondition]): Document = {
    seqDocWithSep(es, "\n", extensionConditionDoc)
  }

  def extensionConditionDoc(e: ExtensionCondition): Document = {
    "%" :: labelDoc(e.extLangName) :: "{" :: text(e.extDefinition) :: text("}")
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
      case vs: ValueSet => valueSetDoc(vs)
      case AnyKind => text(".")
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
      case DisjShapeConstr(shapes) => setDocWithSep(shapes, ",", labelDoc)
      case ConjShapeConstr(shapes) => setDocWithSep(shapes, "|", labelDoc)
      case NotShapeConstr(shape) => "!" :: shapeDoc(s)
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

  def tripleConstraint2String(tc: TripleConstraint)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).tripleConstraintDoc(tc))
  }
  
  def rule2String(r: Rule)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).ruleDoc(r))
  }

  def schema2String(s: SHACLSchema)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).shaclSchemaDoc(s))
  }

  def shape2String(shape: ShapeExpr)(implicit pm: PrefixMap): String =
    prettyPrint(ShaclDoc(pm).shapeExprDoc(shape))

  def rules2String(r: Rule)(implicit pm: PrefixMap): String = {
    prettyPrint(ShaclDoc(pm).ruleDoc(r))
  }

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

