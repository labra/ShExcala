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
  
 def toSchema(ast: SchemaAST): Try[Schema] = {
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

 def shapes2rules(shapes: Map[String,ShapeAST]): Shapes = Undefined
 
 def prefixes2prefixMap(prefixes: Map[String,String]): PrefixMap = Undefined
 
 lazy val bNodeStart = "_:"
 lazy val bNodeStartLength = bNodeStart.length
 
 def toLabel(str:String): Label = {
   if (str.startsWith(bNodeStart)) {
     BNodeLabel(BNodeId(str.drop(bNodeStartLength)))
   } else 
     mkLabel(str)
 }
}
