package es.weso.shacl.json
import AST._
import scala.util._
import es.weso.shacl.Shacl._
import es.weso.shacl._
import es.weso.shacl.PREFIXES._
import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes._
import es.weso.utils.PrefixMapUtils._

object Schema2AST {

  def cnvSchema(schema: Schema): Try[SchemaAST] = {
    Try {
      val ps = cnvPrefix(schema.pm)
      val s = schema.shaclSchema
      SchemaAST.empty.copy(
        prefixes = ps, start = s.start.map(_.toString), startActions = cnvActions(s.startActions), shapes = cnvShapes(s.shapes))
    }
  }

  def cnvShapes(shapes: Map[Label, Shape]): Option[Map[String, ShapeAST]] = {
    if (shapes.isEmpty) None
    else {
      Some(shapes.map {
        case (label, shape) => (label.toString, cnvShape(shape))
      })
    }
  }

  def cnvShape(shape: Shape): ShapeAST = {
    ShapeAST.empty.copy(
      expression = cnvOptShapeExpr(shape.shapeExpr), virtual = cnvBoolean(shape.isVirtual), closed = if (shape.isClosed) Some(true) else None, inherit = cnvLabels(shape.inherit), extra = cnvIRIs(shape.extras), semAct = cnvActions(shape.actions))
  }

  def cnvIRIs(iris: Set[IRI]): Option[List[String]] = {
    if (iris.isEmpty) None
    else Some(iris.map(_.toString).toList)
  }

  def cnvLabels(labels: Set[Label]): Option[List[String]] = {
    if (labels.isEmpty) None
    else Some(labels.map(_.toString).toList)
  }

  def cnvOptShapeExpr(e: ShapeExpr): Option[ExpressionAST] = {
    e match {
      case e: EmptyShape => None
      case _             => Some(cnvShapeExpr(e))
    }
  }

  def cnvShapeExpr(e: ShapeExpr): ExpressionAST = {
    val base = ExpressionAST.empty
    e match {
      case e: EmptyShape => base
      case tc: TripleConstraint => cnvTripleConstraint(tc)
      case Or(id, s1, s2) => {
        base.copy(
          _type = "someOf", id = cnvID(id), expressions = Some(List(cnvShapeExpr(s1), cnvShapeExpr(s2))))
      }
      case XOr(id, s1, s2) => {
        base.copy(
          _type = "oneOf", id = cnvID(id), expressions = Some(List(cnvShapeExpr(s1), cnvShapeExpr(s2))))
      }
      case Group2(id, s1, s2) => {
        base.copy(
          _type = "group", id = cnvID(id), expressions = Some(List(cnvShapeExpr(s1), cnvShapeExpr(s2))))
      }
      case OneOf(id, ss) => {
        base.copy(
          _type = "oneOf", id = cnvID(id), expressions = Some(ss.map(e => cnvShapeExpr(e))))
      }
      case SomeOf(id, ss) => {
        base.copy(
          _type = "someOf", id = cnvID(id), expressions = Some(ss.map(e => cnvShapeExpr(e)).toList))
      }
      case GroupShape(id, ss) => {
        base.copy(
          _type = "group", id = cnvID(id), expressions = Some(ss.map(e => cnvShapeExpr(e)).toList))
      }
      case RepetitionShape(id, s, card, actions) => {
        base.copy(
          _type = "group", id = cnvID(id), expressions = Some(List(cnvShapeExpr(s))), min = cnvMinCard(card), max = cnvMaxCard(card), semAct = cnvActions(actions))
      }
      case IncludeShape(id, label) => {
        base.copy(
          _type = "include", id = cnvID(id), include = Some(label.toString))
      }
    }
  }

  def cnvTripleConstraint(tc: TripleConstraint): ExpressionAST = {
    ExpressionAST.empty.copy(
      _type = "tripleconstraint", id = cnvID(tc.id), value = Some(cnvValueClass(tc.value)), min = cnvMinCard(tc.card), max = cnvMaxCard(tc.card), inverse = cnvBoolean(tc.inverse), negated = cnvBoolean(tc.negated), annotations = cnvAnnotations(tc.annotations), semAct = cnvActions(tc.actions))
  }

  def cnvAnnotations(annotations: List[Annotation]): Option[List[List[String]]] = {
    if (annotations.isEmpty) None
    else {
      Some(annotations.map(a => cnvAnnotation(a)))
    }
  }

  def cnvAnnotation(annotation: Annotation): List[String] = {
    List(annotation.iri.toString, annotation.value.toString)
  }

  def cnvBoolean(b: Boolean): Option[Boolean] = {
    if (b) Some(true)
    else None
  }

  def cnvMinCard(card: Cardinality): Option[Int] =
    if (card == defaultCardinality) None
    else Some(card.getMin)

  def cnvMaxCard(card: Cardinality): Option[Int] =
    card.getMax

  def cnvValueClass(vc: ValueClass): ValueClassAST = {
    val base = ValueClassAST.empty
    vc match {
      case vs: ValueSet => cnvValueSet(vs)
      case dt: Datatype => cnvDatatype(dt)
      case nk: NonLiteralKind => {
        addFacets(nk.facets,
          base.copy(
            nodeKind = Some("nonliteral"), reference = nk.shapeConstr.map(cnvShapeConstr)))
      }
      case ik: IRIKind => addFacets(ik.facets,
        base.copy(
          nodeKind = Some("irikind"), reference = ik.shapeConstr.map(cnvShapeConstr)))
      case lk: LiteralKind => addFacets(lk.facets,
        base.copy(
          nodeKind = Some("literalkind")))
      case bk: BNodeKind => addFacets(bk.facets,
        base.copy(
          nodeKind = Some("bnodekind"), reference = bk.shapeConstr.map(cnvShapeConstr)))
      case sc: ShapeConstr => {
        base.copy(
          reference = Some(cnvShapeConstr(sc)))
      }
      case _ => throw new Exception(s"cnvValueClass: Unsupported conversion, valueClass = $vc")
    }
  }

  def cnvShapeConstr(sc: ShapeConstr): ReferenceAST = {
    sc match {
      case SingleShape(label) =>
        ReferenceAST(Left(label.toString))
      case DisjShapeConstr(shapes) =>
        ReferenceAST(Right(OrAST(shapes.map(_.toString).toList)))
    }
  }

  def cnvDatatype(dt: Datatype): ValueClassAST = {
    addFacets(
      dt.facets,
      ValueClassAST.empty.copy(
        datatype = Some(dt.v.toString)))
  }

  def addFacets(
    facets: List[XSFacet],
    vc: ValueClassAST): ValueClassAST = {
    facets.foldRight(vc) {
      case (f, r) => addFacet(f, r)
    }
  }

  def addFacet(
    f: XSFacet,
    vc: ValueClassAST): ValueClassAST = {
    f match {
      // Numeric facets
      case MinInclusive(n)   => vc.copy(minInclusive = Some(n))
      case MinExclusive(n)   => vc.copy(minExclusive = Some(n))
      case MaxInclusive(n)   => vc.copy(maxInclusive = Some(n))
      case MaxExclusive(n)   => vc.copy(maxExclusive = Some(n))
      case TotalDigits(n)    => vc.copy(totalDigits = Some(n))
      case FractionDigits(n) => vc.copy(fractionDigits = Some(n))

      // String facets
      case Pattern(str)      => vc.copy(pattern = Some(str))
      case Length(n)         => vc.copy(length = Some(n))
      case MinLength(n)      => vc.copy(minLength = Some(n))
      case MaxLength(n)      => vc.copy(maxLength = Some(n))

      case _                 => throw new Exception(s"Unsupported facet $f")
    }
  }

  def cnvValueSet(vs: ValueSet): ValueClassAST = {
    ValueClassAST.empty.copy(
      values =
        if (vs.s.isEmpty) None
        else Some(vs.s.map(v => cnvValue(v)).toList))
  }

  def cnvValue(v: ValueObject): ValueAST = {
    v match {
      case ValueIRI(iri)   => ValueAST(Left(iri.toString))
      case ValueLiteral(l) => ValueAST(Left(l.toString))
      case ValueLang(lang) => throw new Exception(s"cnvValue: Unsupported ValueLang $v")
      case vs: ValueStem   => ValueAST(Right(cnvValueStem(vs)))
      case va: ValueAny    => ValueAST(Right(cnvValueAny(va)))
    }
  }

  def cnvValueStem(v: ValueStem): StemRangeAST = {
    StemRangeAST.empty.copy(
      _type = Some("stemRange"), stem = Some(StemAST(Left(v.stem.toString))), exclusions = cnvExclusions(v.exclusions))
  }

  def cnvValueAny(v: ValueAny): StemRangeAST = {
    val wildCard = WildCardAST(
      _type = Some("wildcard"), 
      stem = None)
    StemRangeAST.empty.copy(
      _type = Some("stemRange"), 
      stem = Some(StemAST(Right(wildCard))),
      exclusions = cnvExclusions(v.exclusions)
    )
  }

  def cnvExclusions(es: List[Exclusion]): Option[List[ExclusionAST]] = {
   if (es.isEmpty) None
   else Some(es map cnvExclusion)
  }
    
    
  def cnvExclusion(ex: Exclusion): ExclusionAST = {
    if (ex.isStem) 
      ExclusionAST(Right(StemAST(Left(ex.iri.toString))))
    else 
      ExclusionAST(Left(ex.iri.toString))
  }  

  def cnvID(id: Option[Label]): Option[String] = {
    id.map(_.toString)
  }
  def cnvActions(as: Actions): Option[Map[String, String]] = {
    if (as.isEmpty) None
    else {
      Some(as.map {
        case (iri, str) => (iri.toString, str)
      })
    }
  }

  def cnvPrefix(pm: PrefixMap): Option[Map[String, String]] = {
    val prefixmap = pm.pm
    if (prefixmap.isEmpty) None
    else {
      Some(prefixmap.mapValues(_.toString))
    }

  }

}
