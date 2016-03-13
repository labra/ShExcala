package es.weso.shacl.converter

import es.weso.shacl._
import es.weso.shacl.Cardinality._
import es.weso.shacl.ValueClass._
import es.weso.rdf.nodes._
import es.weso.rdf._
import scala.util.Try
import es.weso.rdf._
import es.weso.rdf.jena._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.PREFIXES._
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.{
  Logging
}

// TODO: These settings could be read from a properties file
case object Settings {
  val ValueObjectsAsLists = true 
}

case class Schema2RDFException(msg:String) extends Exception(msg)

/**
 * Transforms a Schema to RDF
 */
object Schema2RDF extends Logging {

  /**
   * Transforms a ShEx schema to a RDF
   * 
   * @param schema source schema
   * @param rdf initial RDF builder 
   * @return an RDF builder populated with the shapes from the schema
   */
  def schema2RDF(schema: Schema, rdf: RDFBuilder): RDFBuilder = {
    rdf.addPrefixMap(schema.pm)
    rdf.addPrefix("sh", sh.str)
    rdf.addPrefix("xsd", xsd.str)
    shaclSchema2RDF(schema.shaclSchema, rdf)
    rdf
  }

  private def shaclSchema2RDF(shaclSchema: SHACLSchema, rdf: RDFBuilder): RDFBuilder = {
    val (schemaNode, _) = rdf.createBNode
    // TODO: Maybe, we could include this using a different namespace
    // rdf.addTriple(RDFTriple(schemaNode, rdf_type, sh_Schema))
    rules2RDF(shaclSchema.shapes, schemaNode, rdf)
    start2RDF(shaclSchema.start, schemaNode, rdf)
  }

  private def rules2RDF(
    rules: Map[Label,Shape],
    schemaNode: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    for (rule <- rules) {
      rule2RDF(rule, schemaNode, rdf)
    }
    rdf
  }
  
  private def start2RDF(
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

  private def rule2RDF(
    rule: (Label,Shape),
    schemaNode: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    val ruleNode = rule._1.getNode
    rdf.addTriple(RDFTriple(ruleNode, rdf_type, sh_Shape))
    // TODO: Maybe we could add this to keep track of the schemas
    // rdf.addTriple(RDFTriple(ruleNode, sh_schema, schemaNode))
    shapeDefn2RDF(rule._2, ruleNode, rdf)
//    extensionConditions2RDF(rule.extensionCondition, ruleNode, rdf)
  }

  private def shapeDefn2RDF(
      shape: Shape, 
      shapeNode: RDFNode, 
      rdf: RDFBuilder): RDFBuilder = {
    shape2RDF(shape.shapeExpr, shapeNode,rdf)
  } 
      
  private def shape2RDF(
      shape: ShapeExpr,
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    shape match {
      case t:TripleConstraint => {
        val tripleNode = nodeFromOptionalLabel(t.id,rdf)
        addTriple(rdf,(tripleNode,rdf_type,sh_PropertyConstraint))
        addTriple(rdf,(node,sh_property,tripleNode))
        addTriple(rdf,(tripleNode,sh_predicate,t.iri))
        cardinality2RDF(t.card,tripleNode,rdf)
        value2RDF(t.value,tripleNode,rdf)
        // TODO: Handle rest of properties...negated, inverse,...
        log.info("TripleConstraint: Unhandled inverse, negated...yet")
      }
      
      // TODO: Handle ids
      case EmptyShape(id) => { }
      
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
      case RepetitionShape(id,shape,card,annotations,actions) => {
        val groupLabel = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_group,groupLabel))
        cardinality2RDF(card,groupLabel,rdf)
        shape2RDF(shape,groupLabel,rdf)
//TODO:        annotationsRDF(annotations,node,rdf)
//        actions2RDF(actions,node,rdf)
      }
      case Or(id,shape1,shape2) => {
        val orNode = nodeFromOptionalLabel(id,rdf)
        val (o1, _) = rdf.createBNode
        val (o2, _) = rdf.createBNode
        addTriple(rdf,(node, sh_constraint, orNode))
        addTriple(rdf,(orNode, rdf_type, sh_OrConstraint))
        val list = rdflist(List(o1,o2),rdf)
        rdf.addTriple(RDFTriple(orNode, sh_shapes, list))
        shape2RDF(shape1, o1, rdf)
        shape2RDF(shape2, o2, rdf)
      }
      case SomeOf(id,shapes) => {
        val orNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_constraint,orNode))
        addTriple(rdf,(orNode, rdf_type, sh_OrConstraint))
        // TODO....!!!
        for (shape <- shapes) {
//          shape2RDF(shape,someOfNode,rdf)
        }
      }
/*      case XOr(id,shape1,shape2) => {
        val oneOfNode = nodeFromOptionalLabel(id,rdf)
        addTriple(rdf,(node,sh_oneOf,oneOfNode))
        shape2RDF(shape1,oneOfNode,rdf)
        shape2RDF(shape2,oneOfNode,rdf)
      } 
      case OneOf(id,shapes) => {
        val oneOfNode = nodeFromOptionalLabel(id, rdf)
        addTriple(rdf,(node,sh_someOf,oneOfNode))
        for (shape <- shapes) {
          shape2RDF(shape,oneOfNode,rdf)
        }
      } */
      case _ => {
        log.error(s"schema2RDF: Non supported conversion to RDF. Shape = $shape")
      }
    }
    rdf
  }
  
  private def value2RDF(
      value: ValueClass,
      node: RDFNode,
      rdf: RDFBuilder
      ): RDFBuilder = {
    value match {
      case Datatype(v,facets) => {
        addTriple(rdf,(node,sh_datatype,v))
        facets2RDF(facets,node, rdf)
      }

      // Special treatment for . 
      // Notice that it must be before ValueSet while because it is represented in terms of ValueSet
      case `any` => rdf
      
      case ValueSet(s) => {
        valueSet2RDF(s,node,rdf)
      }
      
      case ValueClassRef(r) => {
        valueClass2RDF(r,node,rdf)
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

  private def valueClass2RDF(r: Label,
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    // TODO: add valueClass reference
    rdf
  }

  private def valueShape2RDF(
      valueShape: ShapeConstr,
      node: RDFNode,
      rdf: RDFBuilder
      ): RDFBuilder = {
    // TODO
    log.info(s"Partially implemented valueShape2RDF: Node = $node, valueShape = $valueShape")
    rdf
  } 
  
  private def valueSet2RDF(
      values: Seq[ValueObject],
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    if (Settings.ValueObjectsAsLists) {
      val (list,_) = nodeList2RDF(values.map{ case v => valueObject2Node(v) },rdf)
      addTriple(rdf,(node,sh_in,list))
    } else {
       for { vo <- values } {
        valueObject2RDF(vo,node,rdf)
       }
       rdf
    }
  }
  
  private def nodeList2RDF(
      nodes: Seq[RDFNode],
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

  private def nodeKind2RDF(
      nodeKind: NodeKind,
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
     nodeKind match {
       // TODO: Add support to shapeConstr and facets
       case IRIKind(shapeContr,facets) => addTriple(rdf,(node,sh_nodeKind,sh_IRI))
       case BNodeKind(shapeConstr,facets) => addTriple(rdf,(node,sh_nodeKind,sh_BNode))     
       case LiteralKind(facets) => addTriple(rdf,(node,sh_nodeKind,sh_Literal)) // TODO: Take into account facets
       case NonLiteralKind(shapeConstr,facets) => addTriple(rdf,(node,sh_nodeKind,sh_NonLiteral))
       case _ => {
         log.error(s"schema2RDF - nodeKind2RDF: Non supported, nodeKind = $nodeKind" )
         rdf
       }
      }
  }
  
  private def valueObject2Node(v: ValueObject): RDFNode = {
    v match {
      case ValueIRI(iri) => iri
      case ValueLiteral(literal) => literal
      case _ => 
        throw Schema2RDFException("Unimplemented ValueObject conversion: " + v)
    }
  }
    
    
  // TODO: Review this code...should add sh_in for a list?
  private def valueObject2RDF(
    value: ValueObject,
    node: RDFNode,
    rdf: RDFBuilder): RDFBuilder = {
    addTriple(rdf,(node,sh_in,valueObject2Node(value)))
    rdf
  }
  
  private def facets2RDF(
      facets: Seq[XSFacet],
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    log.info("Unimplemented facets2RDF...")
    rdf
  }
  
  private def cardinality2RDF(
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
  
  private def inclPropSet2RDF(
      incl: Set[IRI],
      node: RDFNode,
      rdf: RDFBuilder): RDFBuilder = {
    // TODO
    log.error("Unimplemented inclPropSet")
    rdf
  }
  
  private def extensionConditions2RDF(
        ec: Map[Label,String], 
        node: RDFNode, 
        rdf: RDFBuilder): RDFBuilder = {
    /// TODO
    log.error("Unimplemented ExtensionConditions2RDF")
    rdf 
  }

 private def nodeFromOptionalLabel(
     optionalLabel: Option[Label], 
     rdf:RDFBuilder): RDFNode = {
   optionalLabel match {
     case Some(l) => l.getNode
     case None => rdf.createBNode._1
   }
 } 
 
 /**
  * addTriple 
  */
 private def addTriple(rdf: RDFBuilder, triple:(RDFNode, IRI, RDFNode)): RDFBuilder = {
  rdf.addTriple(RDFTriple(triple._1,triple._2,triple._3))
 }

 /**
  * addTriple with an integer literal 
  */
 private def addTripleInteger(rdf: RDFBuilder, triple:(RDFNode, IRI, Integer)): RDFBuilder = {
  rdf.addTriple(RDFTriple(triple._1,triple._2,IntegerLiteral(triple._3)))
 }

 private def rdflist(ls:List[RDFNode],rdf: RDFBuilder): RDFNode = {
    
 def next(x:RDFNode,rest:RDFNode):RDFNode = {
      val (link,_) = rdf.createBNode
      rdf.addTriple(RDFTriple(link,rdf_first,x))
      rdf.addTriple(RDFTriple(link,rdf_rest,rest))
      link
    }
    
    def zero : RDFNode = rdf_nil
    
    ls.foldRight(zero)(next)
  }

}