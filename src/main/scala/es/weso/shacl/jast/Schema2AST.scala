package es.weso.shacl.jast

import scala.util._
import es.weso.shacl.Shacl._
import es.weso.shacl._
import AST._
import es.weso.shacl.PREFIXES._
import es.weso.shacl.Cardinality._
import es.weso.shacl.ValueClass._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.utils.PrefixMapUtils._

/**
 * Converts the internal representation of Schemas defined in [[es.weso.shacl.Schema]] to the abstract syntax tree intended for JSON serialization [[es.weso.shacl.jast.SchemaAST]] 
 */
object Schema2AST {

  def cnvSchema(schema: Schema): Try[SchemaAST] = {
    Try {
      val ps = cnvPrefix(schema.pm)
      val s = schema.shaclSchema
      val vcs = cnvValueClasses(s.valueClasses)
      val shapes = cnvShapes(s.shapes)
      val actions = cnvActions(s.startActions)
      val start = s.start.map(cnvLabel)
      SchemaAST.empty.copy(
        prefixes = ps,
        valueClasses = vcs,
        start = start, 
        startActions = actions, 
        shapes = shapes)
    }
  }

  private def cnvShapes(shapes: Map[Label, Shape]): Option[Map[String, ShapeAST]] = {
    if (shapes.isEmpty) None
    else {
      Some(shapes.map {
        case (label, shape) => (cnvLabel(label), cnvShape(shape))
      })
    }
  }

  private def cnvShape(shape: Shape): ShapeAST = {
    ShapeAST.empty.copy(
      expression = cnvOptShapeExpr(shape.shapeExpr), 
      virtual = cnvBoolean(shape.isVirtual), 
      closed = if (shape.isClosed) Some(true) else None, 
      inherit = cnvLabels(shape.inherit), 
      extra = cnvIRIs(shape.extras), 
      semAct = cnvActions(shape.actions))
  }
  
  private def cnvValueClasses(vcs: Map[Label,ValueClassDefinition]): Option[Map[String,ValueClassAST]] = {
    if (vcs.isEmpty) None
    else Some(vcs.map { case (lbl,vc) => (cnvLabel(lbl), cnvValueClassDef(vc)) })
  }
  
  private def cnvValueClassDef(vcd: ValueClassDefinition): ValueClassAST = ???

  private def cnvIRIs(iris: Seq[IRI]): Option[Seq[String]] = {
    if (iris.isEmpty) None
    else Some(iris map cnvIRI)
  }

  private def cnvLabels(labels: Seq[Label]): Option[Seq[String]] = {
    if (labels isEmpty) None
    else Some(labels map cnvLabel)
  }
  
  private def cnvLabel(label: Label): String = {
    cnvNode(label.getNode)
  }
  
  private def cnvNode(node: RDFNode): String = {
    node match {
      case i: IRI => cnvIRI(i)
      case b: BNodeId => cnvBNode(b)
      case l: Literal => cnvLiteral(l)
    }
  }
  
  private def cnvBNode(b:BNodeId): String = {
    b.toString
  } 

  private def cnvOptShapeExpr(e: ShapeExpr): Option[ExpressionAST] = {
    e match {
      case e: EmptyShape => None
      case _             => Some(cnvShapeExpr(e))
    }
  }

  private def cnvShapeExpr(e: ShapeExpr): ExpressionAST = {
    val base = ExpressionAST.empty
    e match {
      case e: EmptyShape => base
      case tc: TripleConstraint => cnvTripleConstraint(tc)
      case Or(id, s1, s2) => {
        base.copy(
          _type = Some("SomeOf"), 
          expressions = Some(List(cnvShapeExpr(s1), cnvShapeExpr(s2))))
      }
/*      case XOr(id, s1, s2) => {
        base.copy(
          _type = "oneOf", id = cnvID(id), expressions = Some(List(cnvShapeExpr(s1), cnvShapeExpr(s2))))
      } */
      case Group2(id, s1, s2) => {
        base.copy(
          _type = Some("EachOf"), 
          expressions = Some(List(cnvShapeExpr(s1), cnvShapeExpr(s2))))
      }
/*      case OneOf(id, ss) => {
        base.copy(
          _type = "oneOf", id = cnvID(id), expressions = Some(ss.map(e => cnvShapeExpr(e))))
      } */
      case SomeOf(id, ss) => {
        base.copy(
          _type = Some("SomeOf"), expressions = Some(ss.map(e => cnvShapeExpr(e)).toList))
      }
      case GroupShape(id, ss) => {
        base.copy(
          _type = Some("EachOf"), 
          expressions = Some(ss.map(e => cnvShapeExpr(e)).toList))
      }
      case RepetitionShape(id, s, card, annotations, actions) => {
        base.copy(
          _type = Some("EachOf"), 
          expressions = Some(List(cnvShapeExpr(s))), 
          min = cnvMinCard(card), 
          max = cnvMaxCard(card), 
          annotations = cnvAnnotations(annotations),
          semActs = cnvActions(actions))
      }
      case IncludeShape(id, label) => {
        base.copy(
          _type = Some("Include") 
          // TODO: include = Some(cnvLabel(label))
          )
      } 
    }
  }

  private def cnvTripleConstraint(tc: TripleConstraint): ExpressionAST = {
    ExpressionAST.empty.copy(
      _type = Some("TripleConstraint"), 
      predicate = Some(cnvIRI(tc.iri)),
      valueExpr = Some(cnvValueClass(tc.value)), 
      min = cnvMinCard(tc.card), 
      max = cnvMaxCard(tc.card), 
      inverse = cnvBoolean(tc.inverse), 
      negated = cnvBoolean(tc.negated), 
      annotations = cnvAnnotations(tc.annotations), 
      semActs = cnvActions(tc.actions))
  }

  private def cnvAnnotations(annotations: List[Annotation]): Option[List[AnnotationAST]] = {
    if (annotations.isEmpty) None
    else {
      Some(annotations.map(a => cnvAnnotation(a)))
    }
  }

  private def cnvAnnotation(annotation: Annotation): AnnotationAST = {
    AnnotationAST(_type=Some("Annotation"),
        predicate = Some(cnvIRI(annotation.iri)),
        _object = Some(annotation.value.fold(cnvIRI,cnvAnnotationLiteral)))
  }
  
  private def cnvIRI(i:IRI): String = i.str

  private def cnvAnnotationLiteral(l: Literal): String = {
    "\"" + cnvLiteral(l) + "\""
  }
  
  private def cnvLiteral(l: Literal): String = {
    l.toString
  }

  
  private def cnvBoolean(b: Boolean): Option[Boolean] = {
    if (b) Some(true)
    else None
  }

  private def cnvMinCard(card: Cardinality): Option[Int] =
    if (card == defaultCardinality) None
    else Some(card.getMin)

  private def cnvMaxCard(card: Cardinality): Option[MaxAST] =
    if (card == defaultCardinality) None
    else card.getMax match {
      case None => Some(MaxAST(None))
      case Some(n) => Some(MaxAST(Some(n)))
    }

  private def cnvValueClass(vc: ValueClass): ValueClassAST = {
    val base = ValueClassAST.empty.copy(_type = Some("ValueClass"))
    if (vc == any) base
    else
    vc match {
      case vs: ValueSet => cnvValueSet(vs)
      case dt: Datatype => cnvDatatype(dt)
      case nk: NonLiteralKind => {
        addFacets(nk.facets,
          base.copy(
            nodeKind = Some("nonliteral"), 
            reference = nk.shapeConstr.map(cnvShapeConstr)))
      }
      case ik: IRIKind => addFacets(ik.facets,
        base.copy(
          nodeKind = Some("iri"), 
          reference = ik.shapeConstr.map(cnvShapeConstr)))
      case lk: LiteralKind => addFacets(lk.facets,
        base.copy(
          nodeKind = Some("literal")))
      case bk: BNodeKind => addFacets(bk.facets,
        base.copy(
          nodeKind = Some("bnode"), reference = bk.shapeConstr.map(cnvShapeConstr)))
      case sc: ShapeConstr => {
        base.copy(
          reference = Some(cnvShapeConstr(sc)))
      }
      case _ => throw new Exception(s"cnvValueClass: Unsupported conversion, valueClass = $vc")
    }
  }

  private def cnvShapeConstr(sc: ShapeConstr): ReferenceAST = {
    sc match {
      case SingleShape(label) =>
        ReferenceAST(Left(cnvLabel(label)))
      case NotShape(label) =>
        // TODO: Implement NotShape?
        ReferenceAST(Left(cnvLabel(label)))
      case ConjShapeConstr(shapes) =>
        ReferenceAST(Right(Right(AndAST(shapes.map(cnvLabel).toList))))
      case DisjShapeConstr(shapes) =>
        ReferenceAST(Right(Left(OrAST(shapes.map(cnvLabel).toList))))
    }
  }

  private def cnvDatatype(dt: Datatype): ValueClassAST = {
    addFacets(
      dt.facets,
      ValueClassAST.empty.copy(
        datatype = Some(cnvIRI(dt.v))))
  }
  
  private def addFacets(
    facets: List[XSFacet],
    vc: ValueClassAST): ValueClassAST = {
    facets.foldRight(vc) {
      case (f, r) => addFacet(f, r)
    }
  }

  private def addFacet(
    f: XSFacet,
    vc: ValueClassAST): ValueClassAST = {
    f match {
      // Numeric facets
      case MinInclusive(n)   => vc.copy(minInclusive = Some(NumberAST(Left(n))))  // TODO: Check conversion to doubles
      case MinExclusive(n)   => vc.copy(minExclusive = Some(NumberAST(Left(n))))
      case MaxInclusive(n)   => vc.copy(maxInclusive = Some(NumberAST(Left(n))))
      case MaxExclusive(n)   => vc.copy(maxExclusive = Some(NumberAST(Left(n))))
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

  private def cnvValueSet(vs: ValueSet): ValueClassAST = {
    ValueClassAST.empty.copy(
      values =
        if (vs.s.isEmpty) None
        else Some(vs.s.map(v => cnvValue(v)).toList))
  }

  private def cnvValue(v: ValueObject): ValueAST = {
    v match {
      case ValueIRI(iri)   => ValueAST(Left(cnvIRI(iri)))
      case ValueLiteral(l) => ValueAST(Left(cnvLiteral(l)))
      case ValueLang(lang) => throw new Exception(s"cnvValue: Unsupported ValueLang $v")
      case sr: StemRange   => ValueAST(Right(cnvValueStem(sr)))
      case va: ValueAny    => ValueAST(Right(cnvValueAny(va)))
    }
  }

  private def cnvValueStem(v: StemRange): StemRangeAST = {
    StemRangeAST(
      _type= Some("StemRange"),
      stem = Some(StemAST(Left(cnvIRI(v.stem)))), 
      exclusions = cnvExclusions(v.exclusions))
  }

  private def cnvValueAny(v: ValueAny): StemRangeAST = {
    val wildCard = WildCardAST.empty
    StemRangeAST(
      _type = Some("StemRange"),
      stem = Some(StemAST(Right(wildCard))),
      exclusions = cnvExclusions(v.exclusions)
    )
  }

  private def cnvExclusions(es: List[Exclusion]): Option[List[ExclusionAST]] = {
   if (es.isEmpty) None
   else Some(es map cnvExclusion)
  }
    
    
  private def cnvExclusion(ex: Exclusion): ExclusionAST = {
    if (ex.isStem) {
      ExclusionAST(Right(StemAST(Left(cnvIRI(ex.iri)))))
    }
    else 
      ExclusionAST(Left(cnvIRI(ex.iri)))
  }  

  private def cnvID(id: Option[Label]): Option[String] = {
    id.map(cnvLabel)
  }
  
  private def cnvActions(actions: Actions): Option[List[SemActAST]] = {
    if (actions.isEmpty) None
    else {
      Some(actions.toList.map {
        case (iri, str) => 
          SemActAST(
              _type=Some(""), 
              name = Some(cnvIRI(iri)), 
              code = Some(str))
      }.toList)
    }
  }

  private def cnvPrefix(pm: PrefixMap): Option[Map[String, String]] = {
    val prefixmap = pm.pm
    if (prefixmap.isEmpty) None
    else {
      Some(prefixmap.mapValues(cnvIRI))
    }

  }

}
