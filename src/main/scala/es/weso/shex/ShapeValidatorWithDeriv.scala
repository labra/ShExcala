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
import es.weso.monads.Passed
import es.weso.monads.Failure

/**
 * Shape validator using Regular Expression Derivatives
 * Some parts of this code have been inspired by: 
 * https://hackage.haskell.org/package/hxt-regex-xmlschema-9.1.0/docs/src/Text-Regex-XMLSchema-String-Regex.html 
 * 
 */
trait ShapeValidatorWithDeriv extends ShapeValidator with Logging {
  
  override def matchRule(ctx: Context, g: Set[RDFTriple], rule: Rule): Result[Typing] = {
    println("match rule " + rule + " with derivatives. \nTriples: " + g)
    val (dr,ts) = deltaTriples(rule,g,ctx)
    if (nullable(dr)) {
      Passed(ts)    
    } else {
      Failure("Does not match, dr = " + dr)
    }
  }
 
  type Nullable = Boolean
  type Res = (Rule,Stream[Typing])
  lazy val failTyping = Stream()

  def nullable(r: Rule): Nullable = {
    r match {
      case FailRule(_) => false
      case EmptyRule => true
      case ArcRule(_,_,_) => false
      case RevArcRule(_,_,_) => false
      case AndRule(r1,r2) => nullable(r1) && nullable(r2)
      case OrRule(r1,r2) => nullable(r1) || nullable(r2)
      case StarRule(r) => true
      case PlusRule(r) => nullable(r)
      case OptRule(r) => true
      case ActionRule(r,_) => nullable(r)
      case NotRule(r) => !nullable(r) // TODO: check the semantics of this 
      case AnyRule => true
    }
  }
  
  def deltaTriples(r:Rule, ts: Set[RDFTriple], ctx:Context): (Rule,Stream[Typing]) = {
    val e : Res = (r,Stream(ctx.typing))
    def f (b: Res, triple:RDFTriple): Res = {
      val (current,st1) = b
      val (dr,st2) = delta(current,triple,ctx)
      println("Rule " + r )
      println("st1 = " + st1)
      println("st2 = " + st2)
      (dr,st1 ++ st2)
    } 
    ts.foldLeft(e)(f)
  }

  def delta(rule: Rule, triple: RDFTriple, ctx: Context): (Rule, Stream[Typing]) = {
    lazy val noTyping = Stream(ctx.typing)

    rule match {
      case ArcRule(_,n,v) =>
        if (matchName(ctx,triple.pred, n).isValid) {
          val mv = matchValue(ctx,triple.obj,v)
          if (mv.isValid) {
            (EmptyRule,mv.run)
          } else {
            (FailRule("Does not match value " + triple.obj + 
                " with ArcRule " + rule + " Msg: " + mv.failMsg), 
             failTyping)
          }      
        } else {
          (FailRule("Does not match name " + triple.pred + 
              " with ArcRule " + rule), 
              failTyping)
        }
      case RevArcRule(_,n,v) =>
        if (matchName(ctx,triple.pred, n).isValid) {
          val mv = matchValue(ctx,triple.subj,v)
          if (mv.isValid) {
            (EmptyRule,mv.run)
          } else {
            (FailRule("Does not match value " + triple.subj + 
                " with RevArcRule " + rule + " Msg: " + mv.failMsg), 
             failTyping)
          }      
        } else {
          (FailRule("Does not match name " + triple.pred + 
              " with RevArcRule " + rule), 
             failTyping)
        }

      case EmptyRule => 
        (FailRule("Unexpected triple " + triple),failTyping)
        			 
      case f@FailRule(_) => (f,failTyping)
      case OrRule(r1,r2) => { 
        val (dr1,t1) = delta(r1,triple,ctx)
        val (dr2,t2) = delta(r2,triple,ctx)
        (OrRule(dr1,dr2),t1 ++ t2)
      }
      case AndRule(r1,r2) => {
        val (dr1,t1) = delta(r1,triple,ctx)
        if (nullable(r1)) {
          val (dr2,t2) = delta(r2,triple,ctx)
          (OrRule(AndRule(dr1,r2),dr2),t1 ++ t2)
        } else {
          (AndRule(dr1,r2), t1)
        }
      }
      
      case e@StarRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        (AndRule(dr,e),t)
      }
      case OptRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        (dr,t)
      }
      case PlusRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        (AndRule(dr,StarRule(r)),t)
      }
      case ActionRule(r,a) => delta(r,triple,ctx)
      case AnyRule => (EmptyRule,noTyping)
      case NotRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        dr match {
          case FailRule(_) => (EmptyRule,noTyping)
          case _ => (FailRule("Not rule found triple " + t + " that matches"), noTyping)
        }
      }
        
    }
    
    
  }
}