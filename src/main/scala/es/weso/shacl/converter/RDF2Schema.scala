package es.weso.shacl.converter

import es.weso.shacl.XSFacet._
import es.weso.rdf.parser.RDFParser
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util._
import es.weso.rdf._
import es.weso.shacl._
import es.weso.shacl.Label._
import es.weso.shacl.Cardinality._
import es.weso.shacl.PREFIXES._
import es.weso.shacl.ValueClass._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap
import es.weso.utils.{Success => TrySuccess,_}
import es.weso.utils.TryUtils._

case class RDF2SchemaException(msg:String) 
  extends Exception(s"RDF2SchemaException: $msg")

object RDF2Schema 
  extends Logging 
  with RDFParser {
  
  def rdf2Schema(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    val pm = rdf.getPrefixMap
    log.info("RDF2Schema...pm = " + pm)
    for {
      schema <- { 
        val shacl = shaclSchema(rdf)
        shacl 
      }
    } yield (Schema(pm,schema),pm)
  }
  
  def shaclSchema(rdf: RDFReader): Try[SHACLSchema] = {
    // TODO: Check node sh_Graph
    
    val shape_nodes = subjectsWithType(sh_Shape,rdf).toSet
    val shapeClass_nodes = subjectsWithType(sh_ShapeClass,rdf).toSet
    val closedShape_nodes = subjectsWithType(sh_ClosedShape,rdf).toSet
    val with_sh_property_nodes = subjectsWithProperty(sh_property,rdf).toSet
    val shapeCandidates = 
      (shape_nodes ++ 
       shapeClass_nodes ++ 
       closedShape_nodes ++ 
       with_sh_property_nodes).toSeq.distinct // Remove duplicates
    
    log.info("Shape candidates = " + shapeCandidates)
    val maybeRules : Seq[Try[(Label,Shape)]] = 
      shapeCandidates.map{case node => {
      rule(node,rdf)
    }}
    for {
     shapes <- filterSuccess(maybeRules)
     // TODO: Parse Schema label (if any)
     // TODO: Parse start (if any)
    } yield {
     SHACLSchema.empty.copy(shapes = shapes.toMap) 
    }
  }
  
  def rule: RDFParser[(Label,Shape)] = { (n,rdf) => {
    for {
      shape <- shape(n,rdf)
    } yield {
     val lbl = mkLabel(n)
     log.info(s"Shape parsed Label: $lbl: $shape")
     (lbl,shape) 
    }
  }
  }

  // TODO: closed
  // TODO: virtual
  // TODO: extras
  // TODO: inherit
  // TODO: Actions
  def shape: RDFParser[Shape] = { (n,rdf) =>
    for {
      //okTypes <- hasNoRDFType(sh_ClosedShape)(n,rdf)
      //if okTypes
      shape <- {  
         shapeExpr(n,rdf) 
      }
      incls <- inclPropSet(n,rdf)
    } yield Shape.empty.copy(
        shapeExpr = shape,
        inherit = incls)
  }
  
  def inclPropSet: RDFParser[Seq[Label]] = { (n,rdf) =>
    // Todo: 
    Success(Seq())
  } 

/*  def closedShape: RDFParser[ClosedShape] = { (n,rdf) =>
    for {
      checkType <- hasRDFType(sh_ClosedShape)(n,rdf)
      if checkType
      shape <- shapeExpr(n,rdf)
    } yield ClosedShape(shape)
  } */

  def shapeExpr: RDFParser[ShapeExpr] = { (n,rdf) =>
    objectsFromPredicate(sh_property)(n,rdf) match {
      case Success(ps) => {
        log.info("shapeExpr...sh_property = " + ps)
        ps.size match {
          // TODO: check ids
          case 0 => Success(EmptyShape())
          case 1 => {
           val shapeExpr = oneOf(Seq(tripleConstraint))(ps.head,rdf)
           log.info(s"ShapeExpr: $shapeExpr")
           shapeExpr
          }
          case _ => for {
            shapes <- group(shapeExpr,ps.toSeq)(n,rdf)
          } yield GroupShape(None,shapes)
        } 
      }  
      case f => 
        fail("Cannot get objects from predicate " + sh_property + " at node " + n)
    } 
  }
  
  def tripleConstraint: RDFParser[ShapeExpr] = { (n,rdf) =>
    for {
     iri <- iriFromPredicate(sh_predicate)(n,rdf)
     valueClass <- valueClass(n,rdf)
     card <- cardinality(n,rdf)
    } yield {
     val t = TripleConstraint.empty.copy(
         id = Some(mkLabel(n)),
         iri = iri,
         value = valueClass,
         card = card) 
     log.info(s"TripleConstraint: $t") 
     t
    }
  }
  
  def valueClass: RDFParser[ValueClass] = { 
    oneOf(Seq(valueConstr,shapeConstr))
  }
  
  def valueConstr: RDFParser[ValueConstr] = { 
    oneOf(Seq(literalDatatype
             ,nodeKind
             ,valueSet
             ))
  }
  
  def nodeKindFromNode(n: RDFNode): Try[NodeKind] = {
    n match {
      case iri: IRI => nodeKindfromIRI(iri)
      case _ => Failure(throw RDF2SchemaException("Nodekind Value must be an IRI"))
    }
  }
  
  def shapeConstr: RDFParser[ShapeConstr] = { 
    oneOf(Seq(singleShape)) //TODO: Add disjShape, conjShape, notShape...
  }
  
  def singleShape: RDFParser[SingleShape] = { (n,rdf) =>
    for {
      label <- {
       // log.info("looking for single Shape: " + n)
       val obj = objectFromPredicate(sh_valueShape)(n,rdf)
       // log.info("objectFromPredicate valueShape = " + obj)
       obj
      }
    } yield SingleShape(mkLabel(label))
  }
  
  def literalDatatype: RDFParser[Datatype] = { (n,rdf) =>
    for {
      dt <- {
       log.info("literalDatatype: looking for valueType: " + n)
       val obj = objectFromPredicate(sh_datatype)(n,rdf) 
       log.info("objectFromPredicate valueType = " + obj)
       obj
      }
      // TODO: Parse facets and check errors
    } yield {
      if (dt.isIRI)
        Datatype(dt.toIRI,emptyFacets)
      else 
        throw RDF2SchemaException(s"literalDatatype: datatype must be an IRI. node: $n,  rdf: $rdf, datatype: $dt")
    }
  }
  
  // TODO: fallback to (nodeKind AnyKind) if no valueClass is declared 
  def nodeKind: RDFParser[NodeKind] = { (n,rdf) =>
    for {
      nk_iri <- objectFromPredicate(sh_nodeKind)(n,rdf)
      nk <- nodeKindFromNode(nk_iri.toIRI)
    } yield {
      log.info(s"Node kind: $nk")
      nk
    } 
  }
  
  def valueSet: RDFParser[ValueSet] = 
    oneOf(Seq(allowedValue,allowedValues))
  
  def allowedValue: RDFParser[ValueSet] = { (n,rdf) => {
     for {
       shapes <- objectsFromPredicate(sh_allowedValue)(n,rdf)
       if !shapes.isEmpty
     } yield ValueSet(shapes.map(node2valueObject).toSeq)
   }
  }
    
  def allowedValues: RDFParser[ValueSet] = { (n,rdf) => {
     for {
       shapes <-rdfListForPredicate(sh_allowedValues)(n,rdf) 
     } yield ValueSet(shapes.map(node2valueObject))
   }
  }
  
  def node2valueObject(n: RDFNode): ValueObject = {
    n match {
      case lit: Literal => ValueLiteral(lit)
      case iri: IRI => ValueIRI(iri)
      case b: BNodeId => throw RDF2SchemaException("node " + n + " can't be converted to value object") 
    }
  }
  
  
  def cardinality: RDFParser[Cardinality] = { (n,rdf) =>
    if (hasPredicateWithSubject(n,sh_maxCount,rdf)) {
      if (hasPredicateWithSubject(n,sh_minCount,rdf)) {
        rangeCardinality(n,rdf)
      } else {
        rangeCardinalityOnlyMax(n,rdf)
      }
    } else {
      if (hasPredicateWithSubject(n,sh_minCount,rdf)) {
        unboundedCardinalityFrom(n,rdf)
      } else {
        Success(defaultCardinality)
      }
    }
  }

  def rangeCardinalityOnlyMax: RDFParser[RangeCardinality] = { (node,rdf) =>
    for {
      n <- integerLiteralForPredicate(sh_maxCount)(node,rdf)
    ; if (n >= 1)  
    } yield RangeCardinality(1,n)
  }

  def rangeCardinality: RDFParser[RangeCardinality] = { (node,rdf) =>
    for {
      m <- integerLiteralForPredicate(sh_minCount)(node,rdf)
      n <- integerLiteralForPredicate(sh_maxCount)(node,rdf)
    } yield RangeCardinality(m,n)
  }
  
  def unboundedCardinalityFrom: RDFParser[UnboundedCardinalityFrom] = { (node,rdf) =>
    for {
      m <- integerLiteralForPredicate(sh_minCount)(node,rdf)
    } yield UnboundedCardinalityFrom(m)
  }

}
