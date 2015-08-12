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
     val id = None
     val shaclSchema = SHACLSchema(id,rules,start)
     // TODO: Extract startAct 
     Schema(prefixMap, shaclSchema)   
   }
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
       TripleConstraint(id = id, iri = iri, value = value)
     }
   }
 }
 
 def cnvCard(expr: ExpressionAST): Cardinality = {
   val min = expr.min.getOrElse(1)
   if (expr.max.isDefined) {
     RangeCardinality(min,expr.max.get)
   } else {
     UnboundedCardinalityFrom(min)
   }
 }
 
 def cnvValueClass(vc: ValueClassAST): ValueClass = {
   if (vc.values.isDefined) {
     val valueSet = cnvValues(vc.values.get)
     ValueSet(valueSet)
   } else {
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

 def cnvLiteral(l: String): ValueObject = {
   l match {
     case literalDatatype(lex,datatype) => ValueLiteral(DatatypeLiteral(lex,IRI(datatype)))
     case literalLang(lex,lang) => ValueLiteral(LangLiteral(lex,Lang(lang)))
     case literal(lex) => ValueLiteral(StringLiteral(lex))
     case _ => throw new Exception(s"Literal |$l| doesn't match" )
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
