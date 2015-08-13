package es.weso.shacl.converter

import es.weso.shacl._
import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.Try
import es.weso.rdf._
import es.weso.rdf.jena._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.utils.{
  Logging
}

// TODO: These settings could be read from a properties file
case object Settings {
  val ValueObjectsAsLists = true 
}

case class Schema2RDFException(msg:String) extends Exception

object Schema2RDF extends Logging {

  def schema2RDF(schema: Schema, rdf: RDFBuilder): RDFBuilder = {
    rdf.addPrefixMap(schema.pm)
    rdf.addPrefix("sh", sh.str)
    rdf.addPrefix("xsd", xsd.str)
    shaclSchema2RDF(schema.shaclSchema, rdf)
    rdf
  }

  def shaclSchema2RDF(shaclSchema: SHACLSchema, rdf: RDFBuilder): RDFBuilder = {
    val (schemaNode, _) = rdf.createBNode
    rdf.addTriple(RDFTriple(schemaNode, rdf_type, sh_Schema))
    rules2RDF(shaclSchema.shapes, schemaNode, rdf)
    start2RDF(shaclSchema.start, schemaNode, rdf)
  }

  def rules2RDF(
    rules: Map[Label,Shape],
    schemaNode: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    for (rule <- rules) {
      rule2RDF(rule, schemaNode, rdf)
    }
    rdf
  }
  
  def start2RDF(
      start: Option[Label],
      schemaNode: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    start match {
      case None => rdf
      case Some(label) => {
        addTriple(rdf,(schemaNode,sh_start,label.getNode))
      }
    }
  }

  def rule2RDF(
    rule: (Label,Shape),
    schemaNode: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    val ruleNode = rule._1.getNode
//    rdf.addTriple(RDFTriple(ruleNode, rdf_type, sh_Shape))
    rdf.addTriple(RDFTriple(ruleNode, sh_schema, schemaNode))
    shapeDefn2RDF(rule._2, ruleNode, rdf)
//    extensionConditions2RDF(rule.extensionCondition, ruleNode, rdf)
  }

  def shapeDefn2RDF(
      shape: Shape, 
      shapeNode: RDFNode, 
      rdf: RDFBuilder): RDFBuilder = {
    shape2RDF(shape.shapeExpr, shapeNode,rdf)
  } 
      
  def shape2RDF(
      shape: ShapeExpr,
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    shape match {
      case RepetitionShape(id,shape,card) => {
        val node = nodeFromOptionalLabel(id,rdf)
        cardinality2RDF(card,node,rdf)
        shape2RDF(shape,node,rdf)
      }
      case TripleConstraintCard(id,iri,value,card) => {
        val tripleNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(tripleNode,rdf_type,sh_PropertyConstraint))
        addTriple(rdf,(node,sh_property,tripleNode))
        addTriple(rdf,(tripleNode,sh_predicate,iri))
        cardinality2RDF(card,tripleNode,rdf)
        value2RDF(value,tripleNode,rdf)
      }
      case InverseTripleConstraint(id,iri,valueShape) => {
        val tripleNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(tripleNode,rdf_type,sh_InversePropertyConstraint))
        addTriple(rdf,(node,sh_inverseProperty,tripleNode))
        addTriple(rdf,(tripleNode,sh_predicate,iri))
        valueShape2RDF(valueShape,tripleNode,rdf)
      }
      case InverseTripleConstraintCard(id,iri,valueShape,card) => {
        val tripleNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(tripleNode,rdf_type,sh_InversePropertyConstraint))
        addTriple(rdf,(node,sh_inverseProperty,tripleNode))
        addTriple(rdf,(tripleNode,sh_predicate,iri))
        cardinality2RDF(card,tripleNode,rdf)
        valueShape2RDF(valueShape,tripleNode,rdf)
      }
      case EmptyShape => { }
      
      // TODO: Check what to do with the id...
      case Group2(id,shape1,shape2) => {
        shape2RDF(shape1,node,rdf)
        shape2RDF(shape2,node,rdf)
      }
      case GroupShape(id,shapes) => {
        for (shape <- shapes) {
          shape2RDF(shape,node,rdf)
        }
      }
      case RepetitionShape(id,shape,card) => {
        val groupLabel = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_group,groupLabel))
        cardinality2RDF(card,groupLabel,rdf)
        shape2RDF(shape,groupLabel,rdf)
      }
      case Or(id,shape1,shape2) => {
        val someOfNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_someOf,someOfNode))
        shape2RDF(shape1,someOfNode,rdf)
        shape2RDF(shape2,someOfNode,rdf)
      }
      case SomeOfShape(id,shapes) => {
        val someOfNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_someOf,someOfNode))
        for (shape <- shapes) {
          shape2RDF(shape,someOfNode,rdf)
        }
      }
      case XOr(id,shape1,shape2) => {
        val oneOfNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_oneOf,oneOfNode))
        shape2RDF(shape1,oneOfNode,rdf)
        shape2RDF(shape2,oneOfNode,rdf)
      }
      case OneOfShape(id,shapes) => {
        val oneOfNode = nodeFromOptionalLabel(id, rdf)
        addTriple(rdf,(node,sh_someOf,oneOfNode))
        for (shape <- shapes) {
          shape2RDF(shape,oneOfNode,rdf)
        }
      }
    }
    rdf
  }
  
  def value2RDF(
      value: ValueClass,
      node: RDFNode,
      rdf: RDFBuilder
      ): RDFBuilder = {
    value match {
      case LiteralDatatype(v,facets) => {
        addTriple(rdf,(node,sh_valueType,v))
        facets2RDF(facets,node, rdf)
      }
      case ValueSet(s) => {
        valueSet2RDF(s,node,rdf)
      }
      case nk: NodeKind => {
        nodeKind2RDF(nk,node,rdf)
      }
      case vs: ShapeConstr => {
        valueShape2RDF(vs,node,rdf)
      }
    }
    rdf
  }
  
  def valueShape2RDF(
      valueShape: ShapeConstr,
      node: RDFNode,
      rdf: RDFBuilder
      ): RDFBuilder = {
    // TODO
    log.error("Unimplemented valueShape2RDF")
    rdf
  } 
  
  def valueSet2RDF(
      values: Seq[ValueObject],
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    if (Settings.ValueObjectsAsLists) {
      val (list,_) = nodeList2RDF(values.map{ case v => valueObject2Node(v) },rdf)
      addTriple(rdf,(node,sh_allowedValues,list))
    } else {
       for { vo <- values } {
        valueObject2RDF(vo,node,rdf)
       }
       rdf
    }
  }
  
  def nodeList2RDF(nodes: Seq[RDFNode],
      rdf:RDFBuilder): (RDFNode,RDFBuilder) = {
    nodes match {
      case Nil => {
        (rdf_nil,rdf) 
      }
      case (x :: Nil) => {
        val (node,_) = rdf.createBNode
        addTriple(rdf,(node,rdf_first,x))
        addTriple(rdf,(node,rdf_rest,rdf_nil))
        (node,rdf)
      }
      case (x :: xs) => {
        val (node,_) = rdf.createBNode
        addTriple(rdf,(node,rdf_first,x))
        val (rest,_) = nodeList2RDF(xs,rdf)
        addTriple(rdf,(node,rdf_rest,rest))
        (node,rdf)
      }
    }
  }

  def nodeKind2RDF(
      nodeKind: NodeKind,
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
     nodeKind match {
       case IRIKind => addTriple(rdf,(node,sh_nodeKind,sh_IRI))
       case BNodeKind => addTriple(rdf,(node,sh_nodeKind,sh_BNode))     
       case LiteralKind => addTriple(rdf,(node,sh_nodeKind,sh_Literal))
       case NonLiteralKind => addTriple(rdf,(node,sh_nodeKind,sh_NonLiteral))
       case AnyKind => addTriple(rdf,(node,sh_nodeKind,sh_Any))
      }
  }
  
  def valueObject2Node(v: ValueObject): RDFNode = {
    v match {
      case ValueIRI(iri) => iri
      case ValueLiteral(literal) => literal
      case ValueLang(lang) => 
        throw Schema2RDFException("Unimplemented valueLang to Node conversion")
    }
  }
    
    
  def valueObject2RDF(
    value: ValueObject,
    node: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
     addTriple(rdf,(node,sh_allowedValue,valueObject2Node(value)))
    rdf
  }
  
  def facets2RDF(
      facets: Seq[XSFacet],
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    log.info("Unimplemented facets2RDF...")
    rdf
  }
  
  def cardinality2RDF(
      card: Cardinality,
      node: RDFNode,
      rdf: RDFBuilder
      ): RDFBuilder = {
    card match {
      case RangeCardinality(m,n) => {
        addTripleInteger(rdf,(node,sh_minCount,m))
        addTripleInteger(rdf,(node,sh_maxCount,n))
      }
      case UnboundedCardinalityFrom(m) => {
        addTripleInteger(rdf,(node,sh_minCount,m))
      }
    }
    rdf
  }
  
  def inclPropSet2RDF(
      incl: Set[IRI],
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    // TODO
    log.error("Unimplemented inclPropSet")
    rdf
  }
  
  def extensionConditions2RDF(
        ec: Seq[ExtensionCondition], 
        node: RDFNode, 
        rdf: RDFBuilder): RDFBuilder = {
    /// TODO
    log.error("Unimplemented ExtensionConditions2RDF")
    rdf 
  }

 def nodeFromOptionalLabel(optionalLabel: Option[Label], rdf:RDFBuilder): RDFNode = {
   optionalLabel match {
     case Some(l) => l.getNode
     case None => rdf.createBNode._1
   }
 } 
 
 /**
  * addTriple 
  */
 def addTriple(rdf: RDFBuilder, triple:(RDFNode, IRI, RDFNode)): RDFBuilder = {
  rdf.addTriple(RDFTriple(triple._1,triple._2,triple._3))
 }

 /**
  * addTriple with an integer literal 
  */
 def addTripleInteger(rdf: RDFBuilder, triple:(RDFNode, IRI, Integer)): RDFBuilder = {
  rdf.addTriple(RDFTriple(triple._1,triple._2,IntegerLiteral(triple._3)))
 }

}