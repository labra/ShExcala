package es.weso.shacl.json
import AST._
import scala.util._
import es.weso.shacl.Shacl._
import es.weso.shacl._
import es.weso.shacl.PREFIXES._
import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes._



object AST2Schema {
  
 def cnvAST(ast: SchemaAST): Try[Schema] = {
   Try{
     val start = ast.start.map(str => toLabel(str))
     val rules = shapes2rules(ast.shapes.getOrElse(Map()))
     val prefixMap = prefixes2prefixMap(ast.prefixes.getOrElse(Map()))
     val actions = cnvActions(ast.startActions)
     val id = None
     val shaclSchema = SHACLSchema(id,rules,start,actions)
     Schema(prefixMap, shaclSchema)   
   }
 }

 def cnvActions(actions: Option[Map[String,String]]): Map[IRI,String] = {
   val as = actions.getOrElse(Map())
   as.map(cnvAction)
 }
 
 def cnvAction(pair: (String,String)): (IRI,String) = {
   (IRI(pair._1),pair._2)
 }
 
 def shapes2rules(shapes: Map[String,ShapeAST]): Shapes = {
   shapes.map { case (str,shapeAST) => (toLabel(str), cnvShape(shapeAST))}
 }
 
 def cnvShape(shape: ShapeAST): Shape = {
   val shapeExpr = cnvExpr(shape.expression.getOrElse(ExpressionAST.empty))
   val isClosed = shape.closed.getOrElse(false)
   val isVirtual = shape.virtual.getOrElse(false)
   val inherit: Set[Label] = cnvInherit(shape.inherit.getOrElse(List()))
   val extras: Set[IRI] = cnvExtras(shape.extra.getOrElse(List()))
   val actions: Actions = cnvActions(shape.semAct)
   Shape(shapeExpr = shapeExpr, 
       isVirtual = isVirtual, 
       isClosed = isClosed, 
       inherit = inherit,
       extras = extras,
       actions = actions)
 }
 
 def cnvInherit(inh: List[String]): Set[Label]= {
   inh.map{ case str => toLabel(str) }.toSet
 }
 
 def cnvExtras(extra: List[String]): Set[IRI] = {
   extra.map{ case str => IRI(str) }.toSet
 } 
 
 def cnvActions(as: Map[String,String]): Map[IRI,String]= {
   as.map{ case (s1,s2) => (IRI(s1),s2) }
 }
 
 def cnvExpr(expr: ExpressionAST): ShapeExpr = {
   expr._type match {
     case "tripleConstraint" => {
       val iri = IRI(expr.predicate.getOrElse(""))
       val id = expr.id.map(str => toLabel(str))
       val value = expr.value.fold[ValueClass](any)(cnvValueClass)
       val card = cnvCard(expr)
       val inverse = cnvInverse(expr.inverse)
       val negated = cnvNegated(expr.negated)
       val annotations = cnvAnnotations(expr.annotations)
       val actions = cnvActions(expr.semAct)
       TripleConstraint.empty.copy(
           id = id, 
           iri = iri, 
           value = value, 
           card= card, 
           inverse = inverse,
           negated = negated,
           annotations = annotations,
           actions = actions)
     }
     case "oneOf" => {
       val id = expr.id.map(str => toLabel(str))
       val shapes = cnvExpressions(expr.expressions)
       OneOf(id,shapes)
     }
     case "someOf" => {
             val id = expr.id.map(str => toLabel(str))
       val shapes = cnvExpressions(expr.expressions)
       SomeOf(id,shapes)
     }
     case "group" => {
      val id = expr.id.map(str => toLabel(str))
        val shapes = cnvExpressions(expr.expressions)
       if (expr.min.isEmpty && expr.max.isEmpty && expr.semAct.isEmpty)
         GroupShape(id,shapes)
       else {
         val card = cnvCard(expr)
         val actions = cnvActions(expr.semAct)
         RepetitionShape(id,GroupShape(None,shapes),card,actions)
       }
     }
     
     case "" => EmptyShape()
     
     case "include" => {
      val id = expr.id.map(str => toLabel(str)) 
      val label = expr.include.map(str => toLabel(str)).get
      IncludeShape(id,label) 
     } 
     
     case _=> {
       throw new Exception("Unsupported expression conversion " + expr)
     }
   }
 }
 
 def cnvExpressions(expressions: Option[List[ExpressionAST]]): List[ShapeExpr] = {
   expressions.getOrElse(List()).map(cnvExpr)
 }

 def cnvNegated(negated: Option[Boolean]): Boolean = {
   negated.getOrElse(false)
 }

 def cnvInverse(i: Option[Boolean]): Boolean = {
   i.getOrElse(false)
 }
 
 def cnvAnnotations(annotations: Option[List[List[String]]]): List[Annotation]= {
   annotations.getOrElse(List()).map(a => cnvAnnotation(a))
 }
 
 def cnvAnnotation(annotation: List[String]): Annotation = {
   if (annotation.length != 2) throw new Exception("Unsupported annotations without 2 elements: annotation = " + annotation)
   else {
     val iri = cnvIRI(annotation(0))
     val value = cnvValue(annotation(1))
     Annotation(iri,value)
   }
 }
 
 def cnvIRI(str: String): IRI = {
   IRI(str)
 }
 
 def cnvValue(str: String): Either[IRI,Literal] = {
  if (str.startsWith("\"")) {
     Right(cnvLiteral(str))
   } else {
     Left(IRI(str))
   } 
 }

 
 def cnvCard(expr: ExpressionAST): Cardinality = {
   (expr.min, expr.max) match {
     case (None,None) => defaultCardinality
     case (Some(min),None) => UnboundedCardinalityFrom(min)
     case (None,Some(max)) => RangeCardinality(1,max)
     case (Some(min),Some(max)) => RangeCardinality(min,max)
   }
 }
 
 def cnvValueClass(vc: ValueClassAST): ValueClass = {
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
           NonLiteralKind(shapeConstr,facets)
         }
         case "bnode" => {
           val shapeConstr = cnvShapeConstr(vc.reference)
           val facets = collectStringFacets(vc)
           BNodeKind(shapeConstr,facets)
         }
         case "iri" => {
           val shapeConstr = cnvShapeConstr(vc.reference)
           val facets = collectStringFacets(vc)
           IRIKind(shapeConstr, facets)
         }
         case s@_ => throw new Exception(s"Unsupported nodeKind $s")
       }
     } else if (vc.reference.isDefined) {
       cnvShapeConstr(vc.reference).get
     } else if (vc.datatype.isDefined) {
       val facets = collectFacets(vc)
       val node = IRI(vc.datatype.get)
       Datatype(node,facets)
     } else
       any
   }  
 }

  def collectStringFacets(vc: ValueClassAST): List[StringFacet] = {
   List( collectPattern(vc)
       , collectLength(vc)
       , collectMinLength(vc)
       , collectMaxLength(vc)
       ).flatten 
 }

  def collectNumericFacets(vc: ValueClassAST): List[NumericFacet] = {
   List( collectMinInclusive(vc)
       , collectMaxInclusive(vc)
       , collectMinExclusive(vc)
       , collectMaxExclusive(vc)
       , collectTotalDigits(vc)
       , collectFractionDigits(vc)
       ).flatten 
 }
 
 def collectFacets(vc: ValueClassAST): List[XSFacet] = {
   collectNumericFacets(vc) ++ collectStringFacets(vc) 
 }

 // TODO: The following code is quite repetitive...
 // Look for better ways to DRY!!!
 
 def collectLength(vc:ValueClassAST): List[StringFacet] = {
   if (vc.length.isDefined) List(Length(vc.length.get))
   else List()
 }
 
 def collectMinInclusive(vc:ValueClassAST): List[NumericFacet] = {
   if (vc.minInclusive.isDefined) List(MinInclusive(vc.minInclusive.get))
   else List()
 }
 
 def collectMaxInclusive(vc:ValueClassAST): List[NumericFacet] = {
   if (vc.maxInclusive.isDefined) List(MaxInclusive(vc.maxInclusive.get))
   else List()
 }
 
 def collectMinExclusive(vc:ValueClassAST): List[NumericFacet] = {
   if (vc.minExclusive.isDefined) List(MinExclusive(vc.minExclusive.get))
   else List()
 }
 
 def collectMaxExclusive(vc:ValueClassAST): List[NumericFacet] = {
   if (vc.maxExclusive.isDefined) List(MaxExclusive(vc.maxExclusive.get))
   else List()
 }
 
 def collectPattern(vc:ValueClassAST): List[StringFacet] = {
   if (vc.pattern.isDefined) {
    List(Pattern(vc.pattern.get))
   }
   else List()
 }
 
 def collectMinLength(vc:ValueClassAST): List[StringFacet] = {
   if (vc.minLength.isDefined) List(MinLength(vc.minLength.get))
   else List()
 }
 
 def collectMaxLength(vc:ValueClassAST): List[StringFacet] = {
   if (vc.maxLength.isDefined) List(MaxLength(vc.maxLength.get))
   else List()
 }
 
  def collectTotalDigits(vc:ValueClassAST): List[NumericFacet] = {
   if (vc.totalDigits.isDefined) List(TotalDigits(vc.totalDigits.get))
   else List()
 }

  def collectFractionDigits(vc:ValueClassAST): List[NumericFacet] = {
   if (vc.fractionDigits.isDefined) List(FractionDigits(vc.fractionDigits.get))
   else List()
 }
 
 def cnvShapeConstr(ref: Option[ReferenceAST]): Option[ShapeConstr] = {
   ref.map(ref => 
     ref.value match {
     case Left(str) => SingleShape(toLabel(str))
     case Right(OrAST(disjuncts)) => 
       DisjShapeConstr(disjuncts.map(x => toLabel(x)).toSet)  
   })
 }
 
 def cnvValues(values: List[ValueAST]): Seq[ValueObject] = {
   values.map { case value => cnvValue(value) }.toSeq
 }
 
 def cnvValue(v: ValueAST): ValueObject = {
   v.value.fold(cnvString,cnvStemRange)
 }

 def cnvStemRange(s: StemRangeAST): ValueObject = {
   val exclusions = cnvExclusions(s.exclusions)
   val stem = cnvStem(s.stem)
   stem match {
     case Some(iri) => ValueStem(iri,exclusions)
     case None => ValueAny(exclusions) 
   } 
 }
 
 def cnvExclusions(excl: Option[List[ExclusionAST]]): List[Exclusion] = {
   excl.getOrElse(List()).map(cnvExcl)
 }
 
 def cnvExcl(excl: ExclusionAST): Exclusion = {
   excl.value.fold(
       str => Exclusion(IRI(str),false), 
       s => s.value.fold(
           str => Exclusion(IRI(str),false), 
           wc => cnvStemExclusion(wc)
       )
   )
 }

 
 def cnvStem(s: Option[StemAST]): Option[IRI] = {
   if (s.isEmpty) None
   else {
     s.get.value match {
       case Left(str) => Some(IRI(str))
       case Right(_) => None
     }
   }
 }
 
 
 def cnvStemExclusion(s: WildCardAST): Exclusion = {
   s.stem match {
     case Some(StemAST(Left(str))) => Exclusion(IRI(str),true)
     case _ => throw new Exception("cnvStemExclusion: Unsupported conversion..." + s)
   }
 }
 
 def cnvString(s: String): ValueObject = {
   if (s.startsWith("\"")) {
     cnvValueLiteral(s)
   } else {
     ValueIRI(IRI(s))
   }
 }

 lazy val literal = """\"(.*)\"""".r
 lazy val literalDatatype = """\"(.*)\"\^\^(.*)""".r
 lazy val literalLang = """\"(.*)\"@(.*)""".r

 lazy val xsd = "http://www.w3.org/2001/XMLSchema#"
 lazy val xsd_boolean = xsd + "boolean"
 lazy val xsd_integer = xsd + "integer"
 lazy val xsd_decimal = xsd + "decimal"
 lazy val xsd_double = xsd + "double"

 def cnvValueLiteral(l: String): ValueObject = {
   ValueLiteral(cnvLiteral(l))
 }
 
 def cnvLiteral(l: String): Literal = {
   l match {
     case literalDatatype(lex,datatype) => {
       datatype match {
         case `xsd_boolean` => cnvBoolean(lex)
         case `xsd_integer` => cnvInteger(lex)
         case `xsd_decimal` => cnvDecimal(lex)
         case `xsd_double` => cnvDouble(lex)
         case _ => DatatypeLiteral(lex,IRI(datatype)) 
       }
       
     }
     case literalLang(lex,lang) => LangLiteral(lex,Lang(lang))
     case literal(lex) => StringLiteral(lex)
     case _ => throw new Exception(s"Literal |$l| doesn't match" )
   } 
 }

 def cnvInteger(str:String): Literal = {
   IntegerLiteral(Integer.parseInt(str))
 }
 
 def cnvDouble(str:String): Literal = {
   DoubleLiteral(str.toDouble)
 }
 
 def cnvDecimal(str:String): Literal = {
   DecimalLiteral(BigDecimal(str))
 }
 
 def cnvBoolean(str:String): Literal = {
   str match {
     case "false" => BooleanLiteral(false)
     case "true" => BooleanLiteral(true)
     case _ => throw new Exception(s"cnvBoolean: Unsupported value $str")
   }
 }
 
 def prefixes2prefixMap(prefixes: Map[String,String]): PrefixMap = {
   PrefixMap(prefixes.mapValues { x => IRI(x) })
 }
 
 
 lazy val bNodeStart = "_:"
 lazy val bNodeStartLength = bNodeStart.length
 
 def toLabel(str:String): Label = {
   if (str.startsWith(bNodeStart)) {
     BNodeLabel(BNodeId(str.drop(bNodeStartLength)))
   } else 
     mkLabel(str)
 }
}
