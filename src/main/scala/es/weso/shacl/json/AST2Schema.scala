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
       val value = expr.value.fold[ValueClass](AnyKind)(cnvValueClass)
       val card = cnvCard(expr)
       TripleConstraintCard(id = id, iri = iri, value = value, card= card)
     }
     case "oneOf" => {
       val shapes = cnvExpressions(expr.expressions)
       OneOf(None,shapes)
     }
     case "someOf" => {
       val shapes = cnvExpressions(expr.expressions)
       SomeOf(None,shapes)
     }
     case "group" => {
       val shapes = cnvExpressions(expr.expressions)
       if (expr.min.isEmpty && expr.max.isEmpty)
         GroupShape(None,shapes)
       else {
         val card = cnvCard(expr)
         RepetitionShape(None,GroupShape(None,shapes),card)
       }
     }
     
     case "" => EmptyShape
     
     case "include" => throw new Exception("Unsupported include " ) 
     
     case _=> {
       throw new Exception("Unsupported expression conversion " + expr)
     }
   }
 }
 
 def cnvExpressions(expressions: Option[List[ExpressionAST]]): List[ShapeExpr] = {
   expressions.getOrElse(List()).map(cnvExpr)
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
           val facets = collectFacets(vc)
           NonLiteralKind(facets)
         }
         case "bnode" => {
           val facets = collectStringFacets(vc)
           BNodeKind(facets)
         }
         case "iri" => {
           val facets = collectStringFacets(vc)
           IRIKind(facets)
         }
         case s@_ => throw new Exception(s"Unsupported nodeKind $s")
       }
     } else if (vc.reference.isDefined) {
       cnvShapeConstr(vc.reference.get)
     } else
       AnyKind
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
   if (vc.pattern.isDefined) List(Pattern(vc.pattern.get.r))
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
 
 def cnvShapeConstr(ref: ReferenceAST): ShapeConstr = {
   ref.value match {
     case Left(str) => SingleShape(toLabel(str))
     case Right(OrAST(disjuncts)) => 
       DisjShapeConstr(disjuncts.map(x => toLabel(x)).toSet)  
   }
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
     cnvLiteral(s)
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
 
 def cnvLiteral(l: String): ValueObject = {
   l match {
     case literalDatatype(lex,datatype) => {
       datatype match {
         case `xsd_boolean` => cnvBoolean(lex)
         case `xsd_integer` => cnvInteger(lex)
         case `xsd_decimal` => cnvDecimal(lex)
         case `xsd_double` => cnvDouble(lex)
         case _ => ValueLiteral(DatatypeLiteral(lex,IRI(datatype))) 
       }
       
     }
     case literalLang(lex,lang) => ValueLiteral(LangLiteral(lex,Lang(lang)))
     case literal(lex) => ValueLiteral(StringLiteral(lex))
     case _ => throw new Exception(s"Literal |$l| doesn't match" )
   } 
 }

 def cnvInteger(str:String): ValueObject = {
   ValueLiteral(IntegerLiteral(Integer.parseInt(str)))
 }
 
 def cnvDouble(str:String): ValueObject = {
   ValueLiteral(DoubleLiteral(str.toDouble))
 }
 
 def cnvDecimal(str:String): ValueObject = {
   ValueLiteral(DecimalLiteral(BigDecimal(str)))
 }
 
 def cnvBoolean(str:String): ValueObject = {
   str match {
     case "false" => ValueLiteral(BooleanLiteral(false))
     case "true" => ValueLiteral(BooleanLiteral(true))
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
