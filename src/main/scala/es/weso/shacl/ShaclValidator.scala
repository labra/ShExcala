package es.weso.shacl

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shacl.Shacl._
import es.weso.shacl._
import es.weso.shacl.Typing._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.rdf.PrefixMap
import scala.util.parsing.input.Positional
import es.weso.rdf._
import es.weso.shex.Context._
import scala.util.matching.Regex
import es.weso.utils.Logging
import es.weso.utils.Boolean
import scala.util._
// import treelog.LogTreeSyntaxWithoutAnnotations._
import es.weso.rdf.validator.RDFValidator

case class ValidationException(msg:String) extends Exception {
  override def toString = "ValidationException: " + msg 
}

trait ShaclValidator 
 extends Logging {
  
  def matchNodeLabel(
      node: RDFNode, 
      label: Label, 
      ctx: Context
      ): Result[ValidationState] = {
    val triples = ctx.triplesAround(node)
    for {
     shape <- ctx.getShape(label)
     state <- matchTriplesRule(triples,shape,ctx) 
    } yield state 
  }
  
  def matchTriplesRule(
      ts: Set[RDFTriple],
      rule: Rule,
      ctx: Context) : Result[ValidationState] = {
    trace("Match triples: " + ts + " ~ " + rule) 
    rule.shapeDefinition match {
      case OpenShape(shape, propSet) => {
        for {
          vs <- matchTriplesShapeExpr(ts,shape,ctx)
        } yield {
          vs
        }
      }
      case ClosedShape(shape) => {
        for {
          vs <- matchTriplesShapeExpr(ts,shape,ctx)
          _ <- checkNoRemaining(vs)
        } yield vs
      }
    }
  }
  
  def checkNoRemaining(vs: ValidationState): Result[Boolean] = {
    if (vs.hasRemaining) unit(true)
    else failure("Closed shape has remaining triples: " + vs.remaining)
  }
  
  def matchTriplesShapeExpr(
      ts: Set[RDFTriple],
      shape: ShapeExpr,
      ctx: Context): Result[ValidationState] = {
    trace("MatchTriplesShapeExpr " + ts + " ~ " + shape)
    shape match {
      case EmptyShape => 
        if (ts.isEmpty) 
          unit(ValidationState.empty)
          
        else {
          val msg = "EmptyRule: non empty graph"
          log.debug(msg)
          failure(msg)
        }
    
      case tc : TripleConstraint => 
        matchTriples_TripleConstraint(ts,tc,ctx)
        
      case itc : InverseTripleConstraint => 
        matchTriples_InverseTripleConstraint(ts,itc,ctx)
        
      case _ => failure("Unimplemented match triples with shape " + shape)
    }
    
  }
  
  
  def matchTriples_TripleConstraint(
      ts: Set[RDFTriple],
      tc: TripleConstraint,
      ctx: Context): Result[ValidationState] = {
    trace("matchTriples_TripleConstraint, ts= " + ts + " ~ " + tc)
    tc.card match {
      case RangeCardinality(0,0) => 
        noMatchAny(ts,tc,ctx)
      case RangeCardinality(0,n) if n > 0 => { 
        noMatchAny(ts,tc,ctx) orelse 
        matchOneAndContinue(ts,tc,ctx) 
      }
      case RangeCardinality(m,n) if m > 0 && n >= m => {
        matchOneAndContinue(ts,tc,ctx)
      }  
      case RangeCardinality(m,n) if m > 0 && n >= m => {
        matchOneAndContinue(ts,tc,ctx)
      }
      case UnboundedCardinalityFrom(0) => {
        noMatchAny(ts,tc,ctx) orelse
        matchOneAndContinue(ts,tc,ctx)
      }
      case UnboundedCardinalityFrom(m) if m > 0 => {
        matchOneAndContinue(ts,tc,ctx)
      }
      case _ => throw ValidationException("matchTriples_TripleConstraint: Unexpected cardinality")
    }
  }
  
  def matchOneAndContinue(
      ts:Set[RDFTriple],
      tc: TripleConstraint,
      ctx: Context): Result[ValidationState] = {
    for {
      _ <- trace("matchOneAndContinue " + ts + " ~ " + tc)
      (t,ts1) <- removeTriple(ts) 
      _ <- trace("triple " + t)
      _ <- matchPredicate(t.pred, tc.iri)
      typing1 <- matchValueClass(t.obj,tc.value, ctx)
      _ <- trace("typing1 " + typing1)
      state0 <- matchTriples_TripleConstraint(ts1, tc.minusOne, ctx)
      _ <- trace("state0 " + state0)
      state1 <- state0.combineTyping(typing1)
      state2 <- state1.addChecked(t) 
      _ <- trace("state2 " + state0)
    } yield state2
  }
  
  def removeTriple(
      ts: Set[RDFTriple]
      ): Result[(RDFTriple,Set[RDFTriple])] = {
    for {
      _<- trace("Removing triple from " + ts)
      if (!ts.isEmpty)
    } yield (ts.head,ts.tail) 
  }
  
  def matchValueClass(
      obj: RDFNode, 
      v: ValueClass, 
      ctx: Context): Result[Typing] = {
    v match {
      case vc: ValueConstr => for {
        b <- matchValueConstr(obj,vc,ctx)
        if (b)
      } yield ctx.typing
      case sc: ShapeConstr => 
        matchShapeConstr(obj,sc,ctx)
    }
  }

  def matchValueConstr(
      obj: RDFNode,
      v: ValueConstr,
      ctx:Context): Result[Boolean] = {
    v match {
      case vs: ValueSet => matchValueSet(obj,vs,ctx)
      case ld: LiteralDatatype => matchLiteralDatatype(obj,ld,ctx)
      case nk: NodeKind => matchNodeKind(obj,nk,ctx)
    }
  }
  
  def matchValueSet(
      obj : RDFNode,
      values: ValueSet,
      ctx: Context): Result[Boolean] = {
    if (containsNode(obj,values.s)) 
      unit(true)
    else 
      failure("matchValueSet: obj " + obj + " does not match values " + values.s)
  }
  
  def containsNode(
      node: RDFNode, 
      vs: Seq[ValueObject]): Boolean = {
    someTrue(vs.map(vo => matchValueObject(node,vo)))
  }
  
  
  def someTrue(s: Seq[Boolean]): Boolean = {
    s.foldRight(false)((current,next)=> current || next)
  }
  
  def matchValueObject(
      node: RDFNode,
      vo: ValueObject): Boolean = {
    vo match {
      case ValueIRI(iri) => node.isIRI && node.toIRI == iri
      case ValueLiteral(lit) => {
        node match {
          case lit1: Literal => lit == lit1
          case _ => false
        }
      }
      case ValueLang(lang) => {
        node match {
          case lit: LangLiteral => lit.lang == lang
          case _ => false
        }
      }
    }
  }
  
  def matchLiteralDatatype(
      obj: RDFNode,
      ld: LiteralDatatype,
      ctx: Context
      ): Result[Boolean] = {
    obj match {
      case lit: Literal => {
        // TODO...match facets
        if (lit.dataType == ld.v)
          unit(true)
        else failure("matchLiteralDatatype: Literal datatype " + lit.dataType + " doesn't match " + ld.v)
      } 
      case _ => failure("matchLiteralDatatype: obj " + obj + " must be a literal ")
    }
  }
  
  def matchNodeKind(
      obj: RDFNode,
      nk: NodeKind,
      ctx: Context
      ): Result[Boolean] = {
    if (nodeKindOK(obj,nk)) 
      unit(true)
    else 
      failure("matchNodeKind: obj = " + obj + " doesn't match " + nk)
  }   
  
  def nodeKindOK(
      obj: RDFNode,
      nk: NodeKind
      ): Boolean = {
    nk match {
     case IRIKind => obj.isIRI
     case BNodeKind => obj.isBNode
     case LiteralKind => isLiteral(obj)
     case NonLiteralKind => obj.isIRI || obj.isBNode
     case AnyKind => true
     case _ => throw ValidationException("nodeKindOK: unexpected nodeKind" + nk)
    }
  }

  def matchShapeConstr(
      obj: RDFNode,
      v: ShapeConstr,
      ctx: Context): Result[Typing] = {
    throw ValidationException("Unimplemented matchShapeConstr")
  }
  
  def noMatchAny(
      ts: Set[RDFTriple],
      tc: TripleConstraint,
      ctx: Context): Result[ValidationState] = {
    // TODO: Really check that there are no tripels in ts that patch tc
    unit(Pass(ctx.typing,Set(),ts))
  }
  
  def matchPredicate(pred:IRI, expected: IRI):Result[Boolean] = {
    if (pred == expected) unit(true)
    else {
     val msg = "Predicate " + pred + " doesn't match with " + expected
     log.debug("fail: " + msg)
     failure(msg) 
    }
  } 

  def matchTriples_InverseTripleConstraint(
      ts: Set[RDFTriple],
      tc: InverseTripleConstraint,
      ctx: Context): Result[ValidationState] = {
    throw new ValidationException("Unimplemented match_InverseTripleConstraint")
  }
  
  def calculateTyping(n:RDFNode,
      schema: SHACLSchema,
      rdf: RDFGraph): Result[Typing] = {
    failure("unimplemented calculate typing")
  }
/*  
  def matchShapeExpr(
      state: ValidationState,
      ts: Set[RDFTriple], 
      shape: ShapeExpr, 
      rdf:RDFReader):Result[ValidationState] = {
    shape match {
      case TripleConstraint(id,iri,value,card) => for {
        ((ts1,ts2),newState) <- divideByPredicateValue(iri,value,ts)
        if matchCardinality(ts1.size,card)
      } yield 
          newState.copy(
              remaining = ts2, 
              checked = state.checked ++ ts1)
      case _ => 
        throw ValidationException("Unimplemented match ShapeExpr")        
    }
  }
  */
  def divideByPredicateValue(
      iri: IRI, 
      value: ValueClass, 
      ts: Set[RDFTriple]): 
     Result[((Set[RDFTriple], Set[RDFTriple]),ValidationState)] = {
    throw ValidationException("Unimplemented divideByPredicateValue")
  }
  
  def matchCardinality(x: Int, card: Cardinality): Boolean = {
    card match {
      case UnboundedCardinalityFrom(m) => x > m
      case RangeCardinality(m,n) => m <= x && x <= n
    }
  } 
  
  
  def expr(t: Label, s:SHACLSchema): Option[ShapeExpr] = {
    s.findShape(t).map {
      _.shapeDefinition.shape
    }
  }
  
  def incl(t: Label, s: SHACLSchema): Option[Set[IRI]] = {
    s.findShape(t).map {
      case x => x.shapeDefinition match {
        case os:OpenShape => os.inclPropSet
        case _:ClosedShape => Set()
      }
    }
  }
  
  def properties(expr: ShapeExpr): Set[IRI] = {
    expr match {
      case t: TripleConstraint => Set(t.iri)
      case _: InverseTripleConstraint => Set()
      case SomeOfShape(id,shapes) => 
        shapes.map{ case shape => properties(shape)}.flatten.toSet
      case OneOfShape(id,shapes) => 
        shapes.map{ case shape => properties(shape)}.flatten.toSet
      case GroupShape(shapes) => 
        shapes.map{ case shape => properties(shape)}.flatten.toSet
      case RepetitionShape(id,shape,_) => 
        properties(shape)
    } 
  }
  
  def invProperties(expr: ShapeExpr): Set[IRI] = {
    expr match {
      case _: TripleConstraint => Set()
      case t: InverseTripleConstraint => Set(t.iri)
      case SomeOfShape(id,shapes) => 
        shapes.map{ case shape => properties(shape)}.flatten.toSet
      case OneOfShape(id,shapes) => 
        shapes.map{ case shape => properties(shape)}.flatten.toSet
      case GroupShape(shapes) => 
        shapes.map{ case shape => properties(shape)}.flatten.toSet
      case RepetitionShape(id,shape,_) => 
        properties(shape)
    } 
  }

  def triplesSatisfy(neigh: Set[RDFTriple], shape: ShapeExpr, rdf: RDFReader): Boolean = {
    shape match {
      case EmptyShape => neigh.isEmpty
      case TripleConstraint(_,iri,value,card) => {
        throw ValidationException("Unimplemented tripleConstraint")
      }
      case InverseTripleConstraint(_,iri,shapeConstr,card) => {
        throw ValidationException("Unimplemented InverseTripleConstraint")
      }
      case SomeOfShape(id,shapes) => {
        throw ValidationException("Unimplemented SomeOfShapes")
      }
      case OneOfShape(id,shapes) => {
        throw ValidationException("Unimplemented OneOfShapes")
      }
      case GroupShape(shapes) => {
        throw ValidationException("Unimplemented GroupShape")
      }
      case RepetitionShape(id,shape,card) => {
        throw ValidationException("Unimplemented RepetitionShape")
      }
    }
  }

  /**
   *  The typing of a node satisfies a ShapeConstr 
   */
  def satisfies(
      t:Typing, 
      u: RDFNode, 
      sc: ShapeConstr): Boolean = {
    sc match {
      case SingleShape(shape) => t.hasShapes(u) contains shape
      case DisjShapeConstr(shapes) => Boolean.some(shapes.map{ case shape => t.hasShapes(u) contains shape })
      case ConjShapeConstr(shapes) => Boolean.all(shapes.map{ case shape => t.hasShapes(u) contains shape })
      case NotShapeConstr(SingleShape(shape)) => t.hasNegShapesLabels(u) contains shape
      case NotShapeConstr(ConjShapeConstr(shapes)) => Boolean.some(shapes.map{ case shape => t.hasNegShapesLabels(u) contains shape })
      case NotShapeConstr(DisjShapeConstr(shapes)) => Boolean.all(shapes.map{ case shape => t.hasNegShapesLabels(u) contains shape })
      case _ => false
    }
  }
  
  
 def matching(n:RDFNode,t: Typing, constr: TripleConstraint, rdf:RDFReader): Set[RDFTriple] = {
   constr.value match {
     case vc: ValueConstr => matchingValueConstr(n, t, vc, rdf)
     case sc: ShapeConstr => matchingShapeConstr(n, t, sc, rdf)
   }
 }

 def matching(n:RDFNode,t: Typing, constr: InverseTripleConstraint,rdf: RDFReader): Set[RDFTriple] = {
   throw ValidationException("unimplemented matching inverseTripleConstraint")   
 }
 
 def matchingShapeConstr(n: RDFNode, t:Typing, sc: ShapeConstr, rdf: RDFReader) : Set[RDFTriple] = {
   for {
     triple <- outgoing(n,rdf)
     if satisfies(t,triple.obj,sc)
   } yield triple
 }
 
 def matchingValueConstr(n:RDFNode, t:Typing, vc: ValueConstr, rdf:RDFReader): Set[RDFTriple] = {
   for {
     triple <- outgoing(n,rdf)
     if allowed(triple.obj,vc)
   } yield triple
 }
 
 def allowed(obj: RDFNode, vc: ValueConstr): Boolean = {
   vc match {
     case LiteralDatatype(v,facets) => {
       obj match {
         case lit: Literal =>
           // TODO...match facets
           lit.dataType == v && matchFacet(lit,facets) 
         case _ => false
       }
     }
     case ValueSet(s) => {
       s contains obj
     }
     case IRIKind => obj.isIRI
     case BNodeKind => obj.isBNode
     case LiteralKind => isLiteral(obj)
     case NonLiteralKind => obj.isIRI || obj.isBNode
     case AnyKind => true
     case _ => throw ValidationException("allowed: unexpected valueConstr")
   }
 }
 
 // TODO: Substitute this function by a Wesin function
 def isLiteral(n: RDFNode): Boolean = {
   n match {
     case _:Literal => true
     case _ => false
   }
 }
   
 def matchFacet(lit:Literal, facets: Seq[XSFacet]): Boolean = {
   // TODO
   true
 }
 
 def outgoing(n:RDFNode,rdf: RDFReader): Set[RDFTriple] = {
   rdf.triplesWithSubject(n)
 }
 
 def incoming(n:RDFNode,rdf: RDFReader): Set[RDFTriple] = {
   rdf.triplesWithObject(n)
 }
 
/*
  def matchAll(ctx: Context): Result[Typing] = {

    def matchWithTyping(iri: IRI, typing: Typing)(shape: Shape): Result[Typing] = {
      log.debug("matchSome. iri: " + iri.toString +
        "\n--typing: " + typing.toString +
        "\n--shape: " + shape.toString)
      for (t <- matchShape(ctx, iri, shape))
        yield typing.combine(t)
    }

    def matchSomeWithTyping(iri: IRI, typing: Typing): Result[Typing] = {
      log.debug("matchSome. iri: " + iri.toString + ". typing: " + typing.toString)
      Result.passSome(ctx.getShapes, matchWithTyping(iri, typing))
    }

    log.debug("ctx: " + ctx.toString)
    log.debug("iris: " + ctx.getIRIs)
    Result.passAll(ctx.getIRIs, emptyTyping, matchSomeWithTyping)

  }

  def matchShape(ctx: Context, node: RDFNode, rule: Rule): Result[ValidationResult] = {
    log.debug("matchRule, node: " + node + " with rule " + rule.label)
    val triples = ctx.triplesAround(node)
    log.debug("matchRule, triplesAround(" + node + ") = " + triples)
    for (
      ctx1 <- ctx.addTyping(node, rule.label.getNode); t <- matchShape(ctx1, triples, rule.shapeDefinition)
    ) yield t
  }
*/
  def showTriples(g: Set[RDFTriple]): Unit = {
    for (t <- g) {
      log.debug("   " + t)
    }
  }

  def showShape(shape: ShapeExpr): String = {
    ShaclDoc.shape2String(shape)(PrefixMap.empty)
  }

  def matchName(ctx: Context, pred: IRI, iri: IRI): Result[Boolean] = {
    log.debug("MatchName: " + pred + " ~ " + iri)
    if (pred == iri) unit(true)
        else {
          val msg = "matchName: iri=" + pred + " does not match name=" + iri
          log.debug(msg)
          failure(msg)
        }
  }

 
  /**
   * abstract method that can be overriden with different algorithms: derivatives, backtracking, etc.
   */
  def matchShape(
      ctx: Context,
      g: Set[RDFTriple],
      shape: ShapeDefinition): Result[ValidationState] = 
      failure("unimplemented matchShape")
}

