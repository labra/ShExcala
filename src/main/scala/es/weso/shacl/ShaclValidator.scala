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
import es.weso.shacl.ShaclDoc._
import es.weso.utils.PrefixMapUtils._
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
      ): Result[ValidationState] = 
   throw new Exception("Undefined matchNodeLabel")
  
 
/*    val triples = ctx.triplesAround(node)
    for {
     shape <- liftOption(ctx.getShape(label))
     ctx1 <- liftTry(ctx.addTyping(node,label))
     state <- matchTriplesShape(triples,shape,ctx1) 
    } yield state */
  
  def matchTriplesShape(
      ts: List[RDFTriple],
      shape: Shape,
      ctx: Context) : Result[ValidationState] = {
/*    trace("Match triples: " + showTriples(ts)(ctx.pm) + " ~ " + shape2String(shape)(ctx.pm)) 
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
    } */
    ???
  } 
  
  def checkNoRemaining(vs: ValidationState): Result[Boolean] = {
    if (vs.hasRemaining) unit(true)
    else failure("Closed shape has remaining triples: " + vs.remaining)
  }
  
  def matchTriplesShapeExpr(
      ts: List[RDFTriple],
      shape: ShapeExpr,
      ctx: Context): Result[ValidationState] = {
    shape match {
      case e:EmptyShape => 
        if (ts.isEmpty) 
          unit(ValidationState.empty)
          
        else {
          val msg = "EmptyRule: non empty graph"
          log.debug(msg)
          failure(msg)
        }
    
      case tc : TripleConstraint => 
        matchTriples_TripleConstraint(ts,tc,ctx) 
        
        
      case Group2(id,s1,s2) => ???
      /*for {
        (g1, g2) <- parts(ts)
        s1 <- matchTriplesShapeExpr(g1, s1, ctx)
        s2 <- matchTriplesShapeExpr(g2, s2, ctx)
      } yield s1 combine s2 */ 
      
      case Or(id,s1,s2) => 
        matchTriplesShapeExpr(ts, s1, ctx) orelse 
        matchTriplesShapeExpr(ts, s2, ctx)
      
      case XOr(id,s1,s2) => 
        matchTriplesShapeExpr(ts, s1, ctx) xor 
        matchTriplesShapeExpr(ts, s2, ctx)
        
      case group: GroupShape => 
        matchTriplesShapeExpr(ts,toBin(group.shapes,group2,EmptyShape()),ctx)
        
      case oneOf: OneOf => 
        matchTriplesShapeExpr(ts,toBin(oneOf.shapes,xor,EmptyShape()),ctx)
        
      case someOf: SomeOf => 
        matchTriplesShapeExpr(ts,toBin(someOf.shapes,or,EmptyShape()),ctx)
        
      case sc: RepetitionShape => ???
///        matchTriples_ShapeCard(ts,sc.shape,sc.card)(ctx)
        
      case _ => failure("Unimplemented match triples with shape " + shape)
    }
    
  }
  
  def group2(s1: ShapeExpr,s2: ShapeExpr): ShapeExpr = Group2(None,s1,s2)
  def or(s1: ShapeExpr,s2: ShapeExpr): ShapeExpr = Or(None,s1,s2)
  def xor(s1: ShapeExpr,s2: ShapeExpr): ShapeExpr = XOr(None,s1,s2)
  
  def toBin[A](ls: Seq[A], op: (A,A) => A, zero: A): A = {
    ls.foldRight(zero)(op)
  }
  
  def matchTriples_ShapeCard(
       ts: List[RDFTriple],
       sc: ShapeExpr,
       card: Cardinality
      ): Result[ValidationState] = {
     card match {
      case RangeCardinality(0,0) => 
        ??? /*if (ts.isEmpty) unit(Pass(... 
        else unit Fail(...) */
        
      case RangeCardinality(0,n) if n > 0 => ???
      case RangeCardinality(m,n) if m > 0 && n >= m => ??? 
      case RangeCardinality(m,n) if m > 0 && n >= m => ??? 
      case UnboundedCardinalityFrom(0) => ??? 
      case UnboundedCardinalityFrom(m) if m > 0 => ??? 
      case _ => throw ValidationException("matchTriples_TripleConstraint: Unexpected cardinality")
    }
    
  }
  
  def matchTriples_TripleConstraint(
      ts: List[RDFTriple],
      tc: TripleConstraint,
      ctx: Context): Result[ValidationState] = {
    ???
/*    trace("matchTriples_TripleConstraint, ts= " + showTriples(ts)(ctx.pm) + " ~ " + tripleConstraint2String(tc)(ctx.pm))
    for {
      _ <- trace("matchOneAndContinue " + showTriples(ts)(ctx.pm) + " ~ " + tripleConstraint2String(tc)(ctx.pm))
      (t,ts1) <- removeTriple(ts) 
      typing1 <- matchTriple_TripleConstraint(t,tc, ctx)
      // _ <- trace("typing1 " + typing1.showTyping(ctx.pm))
    } yield ??? */ 
  }
  
  type ValidationTyping = Either[ValidationError,Typing]
  
  def matchTriple_TripleConstraint(
      t: RDFTriple, 
      tc: TripleConstraint,
      ctx: Context): Result[ValidationTyping] = {
    ???
/*    if (matchPredicate(t,tc)) {
      for {
        _ <- trace("-> Matching triple " + showTriple(t)(ctx.pm) + " ~ " + tripleConstraint2String(tc)(ctx.pm))
        vt <- matchValueClass(t.obj, tc.value, ctx)
      } yield vt
    } else 
      unit(Left(NoMatchPredicate(t,tc))) */
  }
  
  def removeTriple(
      ts: List[RDFTriple]
      ): Result[(RDFTriple,List[RDFTriple])] = {
    ??? // anyOf(ts)
  }
  
  def matchValueClass(
      obj: RDFNode, 
      v: ValueClass, 
      ctx: Context): Result[ValidationTyping] = {
    v match {
      case vc: ValueConstr => matchValueConstr(obj,vc,ctx)
      case sc: ShapeConstr => matchShapeConstr(obj,sc,ctx)
    }
  }

  def matchValueConstr(
      obj: RDFNode,
      v: ValueConstr,
      ctx:Context): Result[ValidationTyping] = {
    v match {
      case vs: ValueSet => matchValueSet(obj,vs,ctx)
      case ld: Datatype => matchLiteralDatatype(obj,ld,ctx)
      case nk: NodeKind => matchNodeKind(obj,nk,ctx)
    }
  }
  
  def matchValueSet(
      obj : RDFNode,
      values: ValueSet,
      ctx: Context): Result[ValidationTyping] = {
    if (containsNode(obj,values.s)) 
      successTyping(ctx)
    else 
      fail(NoMatchValueSet(obj,values))
  }
  
  def containsNode(
      node: RDFNode, 
      vs: Seq[ValueObject]): Boolean = {
    vs.filter(vo => matchValueObject(node,vo)).size > 0
  }
  
  def matchValueObject(
      node: RDFNode,
      vo: ValueObject): Boolean = {
    vo match {
      case ValueIRI(iri) => 
        node.isIRI && node.toIRI == iri
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

  def successTyping(ctx: Context): Result[ValidationTyping] = {
    unit(Right(ctx.typing))
  }
  
  def fail[A](err: ValidationError): Result[Either[ValidationError,A]] = {
    unit(Left(err))
  }
  
  def matchLiteralDatatype(
      obj: RDFNode,
      ld: Datatype,
      ctx: Context
      ): Result[ValidationTyping] = {
    obj match {
      case lit: Literal => {
        // TODO...match facets
        if (lit.dataType == ld.v)
          successTyping(ctx)
        else 
          fail(NoMatchLiteralDatatype(lit, ld)) 
      } 
      case _ => fail(NoLiteral_MatchLiteralDatatype(obj,ld))
    }
  }
  
  def matchNodeKind(
      obj: RDFNode,
      nk: NodeKind,
      ctx: Context
      ): Result[ValidationTyping] = {
    if (nodeKindOK(obj,nk)) 
      successTyping(ctx) 
    else 
      fail(NoNodeKind(obj,nk))
  }   
  
  def nodeKindOK(
      obj: RDFNode,
      nk: NodeKind
      ): Boolean = {
    nk match {
     // TODO: take into account facets
     case IRIKind(_,facets) => obj.isIRI
     case BNodeKind(_,facets) => obj.isBNode
     case LiteralKind(facets) => isLiteral(obj) 
     case NonLiteralKind(_,facets) => obj.isIRI || obj.isBNode
     case _ => throw ValidationException("nodeKindOK: unexpected nodeKind" + nk)
    }
  }

  def matchShapeConstr(
      obj: RDFNode,
      v: ShapeConstr,
      ctx: Context): Result[ValidationTyping] = {
    v match {
      case SingleShape(label) => 
        if (ctx.containsType(obj,label)) successTyping(ctx)
        else 
          if (ctx.containsNegType(obj,label)) 
            fail(NoTypingFound(obj,label))
          else for {
           vs <- matchNodeLabel(obj,label,ctx)
           typing <- liftTry(vs.getTyping)
          } yield Right(typing)
      case _ => throw ValidationException("Unimplemented matchShapeConstr")
    }
  }
  
  /**
   * checks that all triples with the same predicate match the triple constraint 
   * @param ts set of triples to match
   * @param tc triple constraint
   * @param ctx context
   * @param allowedToFail if true then failures are added to pending list
   */
  def allTriplesWithSamePredicateMatch(
      ts: List[RDFTriple],
      tc: TripleConstraint,
      ctx: Context,
      allowedToFail: Boolean): Result[ValidationState] = {
    
/*    lazy val end : Result[ValidationState] = for {
     _ <- trace("allTriplesWithSamePredicateMatch: End (all passed)")  
    } yield 
      Pass(typing = ctx.typing, remaining = ts, checked = Set(), pending = Set())
    
    ts.foldLeft(end){
      case (result,t) => 
        if (matchPredicate(t, tc)) {
          for {
            _ <- trace("allTriplesWithSamePredicateMatch. triple with same predicate to check: " + t)  
            vt <- matchValueClass(t.obj,tc.value,ctx)
            _ <- trace("allTriplesWithSamePredicate. result: " + vt)
            vs <- result
           } yield {
             vt match {
               case Left(err) => if (allowedToFail) ??? // vs.addPending(t)
                                 else ??? // Fail()
               case Right(typing) => ???
             }
           }
        } 
       else 
           result
    } */
    ???
  }
  
  def matchPredicate(t:RDFTriple, tc: TripleConstraint): Boolean = {
    t.pred == tc.iri
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
      ts: List[RDFTriple]): 
     Result[((List[RDFTriple], List[RDFTriple]),ValidationState)] = {
    throw ValidationException("Unimplemented divideByPredicateValue")
  }
  
  def matchCardinality(x: Int, card: Cardinality): Boolean = {
    card match {
      case UnboundedCardinalityFrom(m) => x > m
      case RangeCardinality(m,n) => m <= x && x <= n
    }
  } 
  
  
  def expr(t: Label, s:SHACLSchema): Option[ShapeExpr] = {
/*    s.findShape(t).map {
      _.shapeDefinition.shape
    } */
    ???
  }
  
  def incl(t: Label, s: SHACLSchema): Option[List[IRI]] = {
/*    s.findShape(t).map {
      case x => x.shapeDefinition match {
        case os:OpenShape => os.inclPropSet
        case _:ClosedShape => Set()
      }
    } */
    ???
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
      case _ => false
    }
  }
  
  
 def matching(n:RDFNode,t: Typing, constr: TripleConstraint, rdf:RDFReader): List[RDFTriple] = {
   constr.value match {
     case vc: ValueConstr => matchingValueConstr(n, t, vc, rdf)
     case sc: ShapeConstr => matchingShapeConstr(n, t, sc, rdf)
   }
 }

 
 def matchingShapeConstr(n: RDFNode, t:Typing, sc: ShapeConstr, rdf: RDFReader) : List[RDFTriple] = {
   for {
     triple <- outgoing(n,rdf)
     if satisfies(t,triple.obj,sc)
   } yield triple
 }
 
 def matchingValueConstr(n:RDFNode, t:Typing, vc: ValueConstr, rdf:RDFReader): List[RDFTriple] = {
   for {
     triple <- outgoing(n,rdf)
     if allowed(triple.obj,vc)
   } yield triple
 }
 
 def allowed(obj: RDFNode, vc: ValueConstr): Boolean = {
   vc match {
     case Datatype(v,facets) => {
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
     // TODO: Take into account facets
     case IRIKind(_,facets) => obj.isIRI
     case BNodeKind(_,facets) => obj.isBNode
     case LiteralKind(facets) => isLiteral(obj) 
     case NonLiteralKind(_,facets) => obj.isIRI || obj.isBNode
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
 
 def outgoing(n:RDFNode,rdf: RDFReader): List[RDFTriple] = {
   rdf.triplesWithSubject(n).toList
 }
 
 def incoming(n:RDFNode,rdf: RDFReader): List[RDFTriple] = {
   rdf.triplesWithObject(n).toList
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
      g: List[RDFTriple],
      shape: Shape): Result[ValidationState] = 
      failure("unimplemented matchShape")
      
}

