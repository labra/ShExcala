package es.weso.shacl.converter

import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util._
import es.weso.rdf._
import es.weso.shacl.Schema
import es.weso.shacl.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap
import es.weso.utils._

case class RDF2SchemaException(msg:String) extends Exception

object RDF2Schema extends Logging {
  
  type RDFParser[a] = (RDFNode, RDFReader) => Try[a]

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
    val shape_nodes = subjectsWithType(sh_Shape,rdf).toSet
    val openShape_nodes = subjectsWithType(sh_OpenShape,rdf).toSet
    val closedShape_nodes = subjectsWithType(sh_ClosedShape,rdf).toSet
    val with_sh_property_nodes = subjectsWithProperty(sh_property,rdf).toSet
    val shapeCandidates = 
      (shape_nodes ++ 
       openShape_nodes ++ 
       closedShape_nodes ++ 
       with_sh_property_nodes).toSeq
    
    log.info("Shape candidates = " + shapeCandidates)
    val maybeRules : Seq[Try[Rule]] = 
      shapeCandidates.map{case node => {
      rule(node,rdf)
    }}
    for {
     rules <- filterSuccess(maybeRules)
     // TODO: Parse Schema label (if any)
     // TODO: Parse start (if any)
    } yield SHACLSchema(None,rules,None)
  }

  // The following code has been taken from this answer: 
  // http://stackoverflow.com/questions/15495678/flatten-scala-try
  // There may be more effcient solutions (but probably less elegant)
  def filterSuccess[A](xs: Seq[Try[A]]): Try[Seq[A]] =
    Try(xs.map(_.get))
  
  def rule: RDFParser[Rule] = { (n,rdf) => {
    for {
      shape <- shapeDefinition(n,rdf)
    } yield Rule(mkLabel(n),shape,Seq())
  }
  }

  def shapeDefinition: RDFParser[ShapeDefinition] = {
    someOf(Seq(openShape,closedShape))
  }

  def openShape: RDFParser[OpenShape] = { (n,rdf) =>
    for {
      //okTypes <- hasNoRDFType(sh_ClosedShape)(n,rdf)
      //if okTypes
      shape <- {  
         shapeExpr(n,rdf) 
      }
      incls <- inclPropSet(n,rdf)
    } yield OpenShape(shape,incls)
  }
  
  def inclPropSet: RDFParser[Set[IRI]] = { (n,rdf) =>
    // Todo: 
    Success(Set())
  }

  def closedShape: RDFParser[ClosedShape] = { (n,rdf) =>
    for {
      checkType <- hasRDFType(sh_ClosedShape)(n,rdf)
      if checkType
      shape <- shapeExpr(n,rdf)
    } yield ClosedShape(shape)
  }

  def shapeExpr: RDFParser[ShapeExpr] = { (n,rdf) =>
    objectsFromPredicate(sh_property)(n,rdf) match {
      case Success(ps) => {
        // println("shapeExpr...sh_property = " + ps)
        ps.size match {
          case 0 => Success(EmptyShape)
          case 1 => {
           // println("Before calling shapeExpr ps.head = " + ps.head)
           val shapeExpr = oneOf(Seq(tripleConstraint))(ps.head,rdf)
           // println("Single ShapeExpr: " + shapeExpr)
           shapeExpr
          }
          case _ => for {
            shapes <- group(shapeExpr,ps.toSeq)(n,rdf)
          } yield GroupShape(shapes)
        } 
      }  
      case f => 
        fail("Cannot get objects from predicate " + sh_property + " at node " + n)
    } 
  }
  
  def tripleConstraint: RDFParser[TripleConstraint] = { (n,rdf) =>
    for {
     iri <- iriObjectFromPredicate(sh_predicate)(n,rdf)
     valueClass <- valueClass(n,rdf)
     card <- cardinality(n,rdf)
    } yield {
     // println("TripleConstraint: " + iri + " " + valueClass + " card: " + card) 
     TripleConstraint(Some(mkLabel(n)),iri,valueClass,card) 
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
  
  def shapeConstr: RDFParser[ShapeConstr] = { 
    oneOf(Seq(singleShape)) //TODO: Add disjShape, conjShape, notShape...
  }
  
  def singleShape: RDFParser[SingleShape] = { (n,rdf) =>
    for {
      label <- {
       // println("looking for single Shape: " + n)
       val obj = objectFromPredicate(sh_valueShape)(n,rdf)
       // println("objectFromPredicate valueShape = " + obj)
       obj
      }
    } yield SingleShape(mkLabel(label))
  }
  
  def literalDatatype: RDFParser[LiteralDatatype] = { (n,rdf) =>
    for {
      dt <- {
       // println("literalDatatype: looking for valueType: " + n)
       val obj = objectFromPredicate(sh_valueType)(n,rdf) 
       // println("objectFromPredicate valueType = " + obj)
       obj
      }
      // TODO: Parse facets
    } yield LiteralDatatype(dt,emptyFacets)
  }
  
  // TODO: fallback to (nodeKind AnyKind) if no valueClass is declared 
  def nodeKind: RDFParser[NodeKind] = { (n,rdf) =>
    for {
      nk_iri <- objectFromPredicate(sh_nodeKind)(n,rdf)
      if (nk_iri.isIRI)
      nk <- nodeKindfromIRI(nk_iri.toIRI)
    } yield nk 
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

  def hasPredicateWithSubject(n:RDFNode,p:IRI,rdf:RDFReader):Boolean = {
    rdf.triplesWithSubjectPredicate(n,p).size > 0
  } 
  
  def iriObjectFromPredicate(p:IRI): RDFParser[IRI] = { (n,rdf) =>
    for {
      node <- objectFromPredicate(p)(n,rdf)
      if (node.isIRI)
    } yield node.toIRI
  }
  
  def objectFromPredicate(p:IRI): RDFParser[RDFNode] = { (n,rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n,p)
    ts.size match {
      case 0 => fail("objectFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => Success(ts.head.obj)
      case _ => fail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
    }        
  }
  
  def objectsFromPredicate(p:IRI): RDFParser[Set[RDFNode]] = { (n,rdf) =>
    val triples = rdf.triplesWithSubjectPredicate(n,p)
    // println("ObjectsFromPredicate " + p + ". node = " + n + " triples =" + triples)
    Success(objectsFromTriples(triples))
  }
  
  def rdfList: RDFParser[List[RDFNode]] = { (n,rdf) =>
    n match {
      case `rdf_nil` => Success(List())
      case x => {
       println("rdfList...n = " + n)
       for {
        elem <- objectFromPredicate(rdf_first)(n,rdf)
        next <- objectFromPredicate(rdf_rest)(n,rdf)
        ls <- rdfList(next,rdf)
      } yield (elem :: ls) 
      }
    }
  }
  
  def rdfListForPredicate(p:IRI): RDFParser[List[RDFNode]] = { (n,rdf) =>
    println("rdfList...predicate..." + p)
    for {
     value <- {
      val v = objectFromPredicate(p)(n,rdf)
      println("value..." + v)
      v
     }
     ls <- { println("value found..." + value)
       rdfList(value,rdf)
      }
    } yield ls
  }
  
  def integerLiteralForPredicate(p: IRI): RDFParser[Integer] = { (n,rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n,p)
    ts.size match {
      case 0 => fail("integerLiteralFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => getIntegerLiteral(ts.head)
      case _ => fail("integerLiteralFromPredicate: More than one value from predicate " + p + " on node " + n)
    }    
  }

  def getIntegerLiteral(t: RDFTriple): Try[Integer] = {
    t.obj match {
      case l: IntegerLiteral => Success(l.int)
      // TODO: case l: DatatypeLiteral(lexicalForm,datatype) => ...
      case _ => fail("getIntegerLiteral: Object " + t.obj + " must be a literal")
    }
  }
  
  def hasNoRDFType(t: IRI): RDFParser[Boolean] = { (n,rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n,rdf)
    } yield !declaredTypes.contains(t)
  }

  def hasRDFType(t: IRI): RDFParser[Boolean] = { (n,rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n,rdf)
    } yield declaredTypes.contains(t)
  }
  
  def hasSomeRDFType(ts: Set[IRI]): RDFParser[Boolean] = { (n,rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n,rdf)
    } yield (declaredTypes.map(_.toIRI).diff(ts)).size > 0
  }

  // TODO: Move the following code to a generic RDFParser 
  // This code could be more generic
  def someOf[A](ps: Seq[RDFParser[A]]): RDFParser[A] = { (n,rdf) => {
    ps.foldLeft(fail("someOf: none of the RDFParsers passed"))
    {
      case ((s : Try[A],parser)) => 
        s match {
         case Success(_) => s
         case Failure(_) => parser(n,rdf)
      }
    }
   }
  }

  def group[A](parser:RDFParser[A], nodes: Seq[RDFNode]): RDFParser[Seq[A]] = { (n,rdf) => {
    val empty : Seq[A] = List() 
    nodes.foldLeft(Success(empty)) {
      case (s,node) => {
        s match {
          case Success(rs) => {
           parser(node,rdf) match {
             case Success(a) => Success(a +: rs)
             case Failure(f) => throw new Exception("group: Unimplemented") // TODO
           } 
          }
          case _ => s
        }
      }
    }
  }
  }

  
  def oneOf[A](parsers: Seq[RDFParser[A]]): RDFParser[A] = { (n,rdf) => {
    val r = parsers.foldLeft(fail("oneOf: none of the RDFParsers passed"))
    {
      case ((current : Try[A],parser)) => 
        current match {
         case Success(_) => {
           parser(n,rdf) match {
             case Success(_) => fail("oneOf: More than one parser passes")
             case Failure(_) => current
           }
         }
         case Failure(_) => parser(n,rdf)
      }
    }
    // println("After oneOf: " + r)
    r
   }
  }

  // TODO: Move this general purpose method elsewhere
  def fail[A](str:String): Try[A] = {
    Failure(new Exception(str))
  }

  def subjectsWithType(t: RDFNode, rdf: RDFReader): Set[RDFNode] = {
    subjectsFromTriples(rdf.triplesWithPredicateObject(rdf_type, t))
  }
  
  def subjectsWithProperty(pred: IRI, rdf:RDFReader): Set[RDFNode] = {
    subjectsFromTriples(rdf.triplesWithPredicate(pred))
  }

  def subjectsFromTriples(triples: Set[RDFTriple]):Set[RDFNode] = {
    triples.map{ case RDFTriple(s,_,_) => s }
  }
  
  def objectsFromTriples(triples: Set[RDFTriple]):Set[RDFNode] = {
    triples.map{ case RDFTriple(_,_,o) => o }
  }
}

