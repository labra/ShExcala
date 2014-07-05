package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.parser.PrefixMap
import es.weso.rdf._
import es.weso.shex.Context._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils.Logging

/**
 * Shape validator using Regular Expression Derivatives
 * Some parts of this code have been inspired by: 
 * https://hackage.haskell.org/package/hxt-regex-xmlschema-9.1.0/docs/src/Text-Regex-XMLSchema-String-Regex.html 
 * 
 */
trait ShapeValidatorWithDeriv extends ShapeValidator with Logging {
 
  type Nullable = Boolean
  type Res = Result[(Rule,Typing)]
  lazy val noTyping = Typing.emptyTyping

  def nullable(r: Rule): Nullable = {
    r match {
      case FailRule(_) => false
      case NoRule => true
      case ArcRule(_,_,_) => false
      case RevArcRule(_,_,_) => false
      case AndRule(r1,r2) => nullable(r1) && nullable(r2)
      case OrRule(r1,r2) => nullable(r1) || nullable(r2)
      case OneOrMore(r) => nullable(r)
      case ActionRule(r,_) => nullable(r)
      case NotRule(r) => !nullable(r) // TODO: check the semantics of this 
      case AnyRule => true
    }
  }
  
  def deltaTriples(r:Rule, ts: RDFTriples, ctx:Context): Result[(Rule,Typing)] = {
    val e : Res = unit((r,Typing.emptyTyping))
    def f (b: Res, t:RDFTriple): Res = ??? // delta(r,t,ctx)
    /// ts.rdfTriples.foldLeft(e)(f)
    ???
  }

  def delta(rule: Rule, triple: RDFTriple, ctx: Context): (Rule, Stream[Typing]) = {
    rule match {
      case ArcRule(_,n,v) =>
        if (matchName(ctx,triple.pred, n).isValid) {
          val mv = matchValue(ctx,triple.obj,v)
          if (mv.isValid) {
            (NoRule,mv.run)
          } else {
            (FailRule("Does not match value " + triple.obj + " with ArcRule " + rule + " Msg: " + mv.failMsg), Stream())
          }      
        } else {
          (FailRule("Does not match name " + triple.pred + " with ArcRule " + rule), Stream())
        }
      case RevArcRule(_,n,v) => ???
      case NoRule => (FailRule("Unexpected triple " + triple),Stream())
        			 
      case f@FailRule(_) => (f,Stream())
      case OrRule(r1,r2) => { 
        val (dr1,t1) = delta(r1,triple,ctx)
        val (dr2,t2) = delta(r2,triple,ctx)
        (OrRule(dr1,dr2),t1 ++ t2)
      }
      case AndRule(r1,r2) => {
        val (dr1,t1) = delta(r1,triple,ctx)
        lazy val (dr2,t2) = delta(r2,triple,ctx)
        if (nullable(dr2)) {
          (OrRule(AndRule(dr1,r2),dr2),t1 ++ t2)
        } else {
          (AndRule(dr1,r2), t1)
        }
      }
      case OneOrMore(r) => {
        val (dr,t1) = delta(r,triple,ctx)
        (AndRule(dr,OneOrMore(r)),t1)
      }
      case ActionRule(r,a) => ???
      case AnyRule => ???
      case NotRule(r) => ???
    }
    
    
  }
}