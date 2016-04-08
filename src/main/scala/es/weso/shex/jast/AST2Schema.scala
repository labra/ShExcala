package es.weso.shex.jast

import scala.util._
import es.weso.shex._
import es.weso.shex.PREFIXES._
import es.weso.shex.Cardinality._
import es.weso.shex.ValueClass._
import es.weso.shex.Label._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import AST._

/**
 * Represents conversion exceptions between AST and Schema 
 */
case class AST2SchemaException(msg: String) extends Exception(s"ASTSchemaException: $msg")

object AST2Schema {

  /**
   * Tries to converts a [[es.weso.shacl.jast.SchemaAST]] to a [[es.weso.shacl.Schema]]
   */
  def cnvAST(ast: SchemaAST): Try[Schema] = {
    Try {
      val start = ast.start.map(str => toLabel(str))
      val valueClasses = cnvValueClasses(ast.valueClasses)
      val rules = shapes2rules(ast.shapes.getOrElse(Map()))
      val prefixMap = prefixes2prefixMap(ast.prefixes.getOrElse(Map()))
      val actions = cnvActions(ast.startActions)
      val id = None
      val shaclSchema = ShExSchema(id, valueClasses, rules, start, actions)
      Schema(prefixMap, shaclSchema)
    }
  }

  private def cnvValueClasses(maybe_vcs: Option[Map[String, ValueClassAST]]): Map[Label, ValueClassDefinition] = {
    maybe_vcs.getOrElse(Map()).map { case (str, vc) => (toLabel(str), cnvValueClassDef(vc)) }
  }

  private def cnvActions(actions: Option[Seq[SemActAST]]): Actions = {
    val as = actions.getOrElse(Seq())
    Actions.fromList(as.toList.map(cnvAction))
  }

  private def cnvAction(a: SemActAST): (IRI, String) = {
    (IRI(a.name.getOrElse("")), a.code.getOrElse(""))
  }

  private def shapes2rules(shapes: Map[String, ShapeAST]): Map[Label, Shape] = {
    shapes.map { case (str, shapeAST) => (toLabel(str), cnvShape(shapeAST)) }
  }

  private def cnvShape(shape: ShapeAST): Shape = {
    val shapeExpr = cnvExpr(shape.expression.getOrElse(ExpressionAST.empty))
    val isClosed = shape.closed.getOrElse(false)
    val isVirtual = shape.virtual.getOrElse(false)
    val inherit = cnvInherit(shape.inherit.getOrElse(Seq()))
    val extras = cnvExtras(shape.extra.getOrElse(Seq()))
    val actions = cnvActions(shape.semAct)
    Shape(shapeExpr = shapeExpr,
      isVirtual = isVirtual,
      isClosed = isClosed,
      inherit = inherit,
      extras = extras,
      actions = actions)
  }

  private def cnvInherit(inh: Seq[String]): Seq[Label] = {
    inh.map { case str => toLabel(str) }
  }

  private def cnvExtras(extra: Seq[String]): Seq[IRI] = {
    extra.map { case str => IRI(str) }
  }

  private def cnvActions(as: Map[String, String]): Map[IRI, String] = {
    as.map { case (s1, s2) => (IRI(s1), s2) }
  }

  private def cnvExpr(expr: ExpressionAST): ShapeExpr = {
    if (!expr._type.isDefined) EmptyShape()
    else
    expr._type.get match {
      case "TripleConstraint" => {
        val iri = IRI(expr.predicate.getOrElse(""))
        //       val id = expr.id.map(str => toLabel(str))
        val value =
          /*         if (expr.valueExpr.isDefined) 
           cnvValueClassRef(expr.valueExpr.get)
         else */
          expr.valueExpr.fold[ValueClass](any)(cnvValueClass)
        val card = cnvCard(expr)
        val inverse = cnvInverse(expr.inverse)
        val negated = cnvNegated(expr.negated)
        val annotations = cnvAnnotations(expr.annotations)
        val actions = cnvActions(expr.semActs)
        TripleConstraint.empty.copy(
          //           id = id, 
          iri = iri,
          value = value,
          card = card,
          inverse = inverse,
          negated = negated,
          annotations = annotations,
          actions = actions)
      }
      /*     case "oneOf" => {
       val id = expr.id.map(str => toLabel(str))
       val shapes = cnvExpressions(expr.expressions)
       OneOf(id,shapes)
     }*/
      case "SomeOf" => {
        //       val id = expr.id.map(str => toLabel(str))
        val shapes = cnvExpressions(expr.expressions)
        SomeOf(None, shapes)
      }
      case "EachOf" => {
        //      val id = expr.id.map(str => toLabel(str))
        val shapes = cnvExpressions(expr.expressions)
        if (expr.min.isEmpty && expr.max.isEmpty && expr.semActs.isEmpty)
          GroupShape(None, shapes)
        else {
          val card = cnvCard(expr)
          val actions = cnvActions(expr.semActs)
          val annotations = cnvAnnotations(expr.annotations)
          if (shapes.length == 1)
            RepetitionShape(None, shapes.head, card, annotations, actions)
          else
            RepetitionShape(None, GroupShape(None, shapes), card, annotations, actions)
        }
      }

      case "" => EmptyShape()

      //     case "include" => {
      //      val id = expr.id.map(str => toLabel(str)) 
      //      val label = expr.include.map(str => toLabel(str)).get
      //      IncludeShape(None,label) 
      //     } 

      case _ => {
        throw AST2SchemaException("Unsupported expression conversion " + expr)
      }
    }
  }

  private def cnvExpressions(expressions: Option[List[ExpressionAST]]): List[ShapeExpr] = {
    expressions.getOrElse(List()).map(cnvExpr)
  }

  private def cnvNegated(negated: Option[Boolean]): Boolean = {
    negated.getOrElse(false)
  }

  private def cnvInverse(i: Option[Boolean]): Boolean = {
    i.getOrElse(false)
  }

  private def cnvAnnotations(annotations: Option[List[AnnotationAST]]): List[Annotation] = {
    annotations.getOrElse(List()).map(a => cnvAnnotation(a))
  }

  private def cnvAnnotation(annotation: AnnotationAST): Annotation = {
    val iri = cnvIRI(annotation.predicate.getOrElse(""))
    val value = cnvValue(annotation._object.getOrElse(""))
    Annotation(iri, value)
  }

  private def cnvIRI(str: String): IRI = {
    IRI(str)
  }

  private def cnvValueClassRef(vcr: String): ValueClass = {
    ValueClassRef(labelStr(vcr))
  }

  private def cnvValue(str: String): Either[IRI, Literal] = {
    if (str.startsWith("\"")) {
      Right(cnvLiteral(str))
    } else {
      Left(IRI(str))
    }
  }

  private def cnvCard(expr: ExpressionAST): Cardinality = {
    (expr.min, expr.max) match {
      case (None, None)           => defaultCardinality
      case (Some(min), None)      => UnboundedCardinalityFrom(min)
      case (None, Some(max))      => cnvMax(1, max)
      case (Some(min), Some(max)) => cnvMax(min, max)
    }
  }

  private def cnvMax(min: Int, max: MaxAST): Cardinality = {
    max.v match {
      case None      => UnboundedCardinalityFrom(min)
      case Some(num) => RangeCardinality(min, num)
    }
  }
  
  private def cnvValueClassDef(vc: ValueClassAST): ValueClassDefinition = {
    ValueClassDefinition.fromValueClass(cnvValueClass(vc))
  }

  private def cnvValueClass(vc: ValueClassAST): ValueClass = {
    if (vc.values.isDefined) {
      val valueSet = cnvValues(vc.values.get)
      ValueSet(valueSet)
    } else {
      if (vc.nodeKind.isDefined) {
        vc.nodeKind.get match {
          case "literal" => {
            val facets = collectFacets(vc)
            LiteralKind(facets)
          }
          case "nonliteral" => {
            val shapeConstr = cnvShapeConstr(vc.reference)
            val facets = collectFacets(vc)
            NonLiteralKind(shapeConstr, facets)
          }
          case "bnode" => {
            val shapeConstr = cnvShapeConstr(vc.reference)
            val facets = collectStringFacets(vc)
            BNodeKind(shapeConstr, facets)
          }
          case "iri" => {
            val shapeConstr = cnvShapeConstr(vc.reference)
            val facets = collectStringFacets(vc)
            IRIKind(shapeConstr, facets)
          }
          case s @ _ => throw AST2SchemaException(s"Unsupported nodeKind $s")
        }
      } else if (vc.reference.isDefined) {
        cnvShapeConstr(vc.reference).get
      } else if (vc.datatype.isDefined) {
        val facets = collectFacets(vc)
        val node = IRI(vc.datatype.get)
        Datatype(node, facets)
      } else
        ValueClass.any
    }
  }

  private def collectStringFacets(vc: ValueClassAST): List[StringFacet] = {
    List(collectPattern(vc), collectLength(vc), collectMinLength(vc), collectMaxLength(vc)).flatten
  }

  private def collectNumericFacets(vc: ValueClassAST): List[NumericFacet] = {
    List(collectMinInclusive(vc), collectMaxInclusive(vc), collectMinExclusive(vc), collectMaxExclusive(vc), collectTotalDigits(vc), collectFractionDigits(vc)).flatten
  }

  private def collectFacets(vc: ValueClassAST): List[XSFacet] = {
    collectNumericFacets(vc) ++ collectStringFacets(vc)
  }

  // TODO: The following code is quite repetitive...
  // Look for better ways to DRY!!!

  private def collectLength(vc: ValueClassAST): List[StringFacet] = {
    if (vc.length.isDefined) List(Length(vc.length.get))
    else List()
  }

  private def collectMinInclusive(vc: ValueClassAST): List[NumericFacet] = {
    if (vc.minInclusive.isDefined) List(MinInclusive(cnvNumber(vc.minInclusive.get)))
    else List()
  }

  private def cnvNumber(n: NumberAST): Integer = {
    n.v match {
      case Left(n)  => n
      case Right(n) => throw new Exception("cnvNumber: Unsupported conversion from double to Number")
    }
  }

  private def collectMaxInclusive(vc: ValueClassAST): List[NumericFacet] = {
    if (vc.maxInclusive.isDefined) List(MaxInclusive(cnvNumber(vc.maxInclusive.get)))
    else List()
  }

  private def collectMinExclusive(vc: ValueClassAST): List[NumericFacet] = {
    if (vc.minExclusive.isDefined) List(MinExclusive(cnvNumber(vc.minExclusive.get)))
    else List()
  }

  private def collectMaxExclusive(vc: ValueClassAST): List[NumericFacet] = {
    if (vc.maxExclusive.isDefined) List(MaxExclusive(cnvNumber(vc.maxExclusive.get)))
    else List()
  }

  private def collectPattern(vc: ValueClassAST): List[StringFacet] = {
    if (vc.pattern.isDefined) {
      List(Pattern(vc.pattern.get))
    } else List()
  }

  private def collectMinLength(vc: ValueClassAST): List[StringFacet] = {
    if (vc.minLength.isDefined) List(MinLength(vc.minLength.get))
    else List()
  }

  private def collectMaxLength(vc: ValueClassAST): List[StringFacet] = {
    if (vc.maxLength.isDefined) List(MaxLength(vc.maxLength.get))
    else List()
  }

  private def collectTotalDigits(vc: ValueClassAST): List[NumericFacet] = {
    if (vc.totalDigits.isDefined) List(TotalDigits(vc.totalDigits.get))
    else List()
  }

  private def collectFractionDigits(vc: ValueClassAST): List[NumericFacet] = {
    if (vc.fractionDigits.isDefined) List(FractionDigits(vc.fractionDigits.get))
    else List()
  }

  private def cnvShapeConstr(ref: Option[ReferenceAST]): Option[ShapeConstr] = {
    ref.map(ref =>
      ref.value match {
        case Left(str) => SingleShape(toLabel(str))
        case Right(Left(OrAST(disjuncts))) =>
          DisjShapeConstr(disjuncts.map(x => toLabel(x)))
        case Right(Right(AndAST(conjuncts))) =>
          ConjShapeConstr(conjuncts.map(x => toLabel(x)))
      })
  }

  private def cnvValues(values: List[ValueAST]): Seq[ValueObject] = {
    values.map { case value => cnvValue(value) }.toSeq
  }

  private def cnvValue(v: ValueAST): ValueObject = {
    v.value.fold(cnvString, cnvStemRange)
  }

  private def cnvStemRange(s: StemRangeAST): ValueObject = {
    // TODO: Review if we really need all the following checks
    val exclusions = cnvExclusions(s.exclusions)
    s.stem match {
      case Some(st) => {
        val stem = cnvStem(st)
        stem match {
          case Some(iri) => StemRange(iri, exclusions)
          case None      => ValueAny(exclusions)
        }
      }
      case None => ValueAny(exclusions)
    }
  }

  private def cnvExclusions(excl: Option[List[ExclusionAST]]): List[Exclusion] = {
    excl.getOrElse(List()).map(cnvExcl)
  }

  private def cnvExcl(excl: ExclusionAST): Exclusion = {
    excl.value.fold(
      str => Exclusion(IRI(str), false),
      s => s.value.fold(
        str => Exclusion(IRI(str), true),
        wc => cnvStemExclusion(wc)))
  }

  private def cnvStem(s: StemAST): Option[IRI] = {
    //   if (s.isEmpty) None
    //   else {
    s.value match {
      case Left(str) => Some(IRI(str))
      case Right(_)  => None
    }
    //  }
  }

  private def cnvStemExclusion(s: WildCardAST): Exclusion = {
    s.stem match {
     case Some(StemAST(Left(str))) => Exclusion(IRI(str),true)
     case _ => throw AST2SchemaException("cnvStemExclusion: Unsupported conversion..." + s)
   } 
  } 

  private def cnvString(s: String): ValueObject = {
    if (s.startsWith("\"")) {
      cnvValueLiteral(s)
    } else {
      ValueIRI(IRI(s))
    }
  }

  private lazy val literal = """\"(.*)\"""".r
  private lazy val literalDatatype = """\"(.*)\"\^\^(.*)""".r
  private lazy val literalLang = """"(.*)"@(.*)""".r

  private lazy val xsd = "http://www.w3.org/2001/XMLSchema#"
  private lazy val xsd_boolean = xsd + "boolean"
  private lazy val xsd_integer = xsd + "integer"
  private lazy val xsd_decimal = xsd + "decimal"
  private lazy val xsd_double = xsd + "double"

  private def cnvValueLiteral(l: String): ValueObject = {
    ValueLiteral(cnvLiteral(l))
  }

  private def cnvLiteral(l: String): Literal = {
    l match {
      case literalDatatype(lex, datatype) => {
        datatype match {
          case `xsd_boolean` => cnvBoolean(lex)
          case `xsd_integer` => cnvInteger(lex)
          case `xsd_decimal` => cnvDecimal(lex)
          case `xsd_double`  => cnvDouble(lex)
          case _             => DatatypeLiteral(lex, IRI(datatype))
        }

      }
      case literalLang(lex, lang) => {
        val lit = LangLiteral(lex, Lang(lang))
        lit
      }
      case literal(lex) => {
        StringLiteral(lex)
      }
      case _ => throw AST2SchemaException(s"Literal |$l| doesn't match")
    }
  }

  private def cnvInteger(str: String): Literal = {
    IntegerLiteral(Integer.parseInt(str))
  }

  private def cnvDouble(str: String): Literal = {
    DoubleLiteral(str.toDouble)
  }

  private def cnvDecimal(str: String): Literal = {
    DecimalLiteral(BigDecimal(str))
  }

  private def cnvBoolean(str: String): Literal = {
    str match {
      case "false" => BooleanLiteral(false)
      case "true"  => BooleanLiteral(true)
      case _       => throw AST2SchemaException(s"cnvBoolean: Unsupported value $str")
    }
  }

  private def prefixes2prefixMap(prefixes: Map[String, String]): PrefixMap = {
    PrefixMap(prefixes.mapValues { x => IRI(x) })
  }

  lazy val bNodeStart = "_:"
  lazy val bNodeStartLength = bNodeStart.length

  private def toLabel(str: String): Label = {
    if (str.startsWith(bNodeStart)) {
      BNodeLabel(BNodeId(str.drop(bNodeStartLength)))
    } else
      labelStr(str)
  }
}
