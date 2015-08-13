package es.weso.shacl.json
import AST._
import scala.util._
import es.weso.shacl.Shacl._
import es.weso.shacl._
import es.weso.shacl.PREFIXES._
import es.weso.rdf.PrefixMap
import es.weso.rdfgraph.nodes._



object AST2Schema {
  
 def Undefined : Nothing = {
   throw new Exception("Undefined") 
 } 
  
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
   val inherit: Set[IRI] = cnvInherit(shape.inherit.getOrElse(List()))
   Shape(shapeExpr = shapeExpr, isClosed = isClosed, inherit = inherit)
 }
 
 def cnvInherit(inh: List[String]): Set[IRI]= {
   inh.map{ case str => IRI(str) }.toSet
 }
 
 def cnvExpr(expr: ExpressionAST): ShapeExpr = {
   expr._type match {
     case "tripleConstraint" => {
       val iri = IRI(expr.predicate)
       val id = expr.id.map(str => mkLabel(str))
       val value = cnvValueClass(expr.value)
       val card = cnvCard(expr)
       TripleConstraintCard(id = id, iri = iri, value = value, card= card)
     }
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
         case "literal" => LiteralKind
         case "bnode" => BNodeKind
         case "iri" => IRIKind
         case s@_ => throw new Exception(s"Unsupported nodeKind $s")
       }
     } else
       AnyKind
   }  
 }
 
 def cnvValues(values: List[ValueAST]): Seq[ValueObject] = {
   values.map { case value => cnvValue(value) }.toSeq
 }
 
 def cnvValue(v: ValueAST): ValueObject = {
   v.value.fold(cnvString,cnvStem)
 }
 
 def cnvStem(s: StemAST): ValueObject = Undefined
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
