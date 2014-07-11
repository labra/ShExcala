package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.monads.Result._
import es.weso.parser.PrefixMap
import es.weso.rdf._
import es.weso.shex.Context._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils.Logging
import es.weso.monads._

/**
 * Shape validator using Regular Expression Derivatives
 * Some parts of this code have been inspired by: 
 * https://hackage.haskell.org/package/hxt-regex-xmlschema-9.1.0/docs/src/Text-Regex-XMLSchema-String-Regex.html 
 * 
 */
trait ShapeValidatorWithDeriv extends ShapeValidator with Logging {
  
  override def id = "Validator by Derivatives 1.0"
  
  implicit val pm: PrefixMap = PrefixMaps.commonShex
  
  override def matchRule(ctx: Context, g: Set[RDFTriple], rule: Rule): Result[Typing] = {
    val (dr,ts) = deltaTriples(rule,g,ctx)
    if (nullable(dr)) {
      Passed(ts)    
    } else {
      Failure("Does not match, dr = " + showRule(dr)(ctx.pm))
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
      case RelationRule(_,_,_) => false
      case AndRule(r1,r2) => nullable(r1) && nullable(r2)
      case OrRule(r1,r2) => nullable(r1) || nullable(r2)
      case StarRule(r) => true
      case PlusRule(r) => nullable(r)
      case OptRule(r) => true
      case ActionRule(r,_) => nullable(r)
      case RangeMinRule(m,r) => m == 0 || nullable(r)
      case RangeRule(m,n,r) => m == 0 || nullable(r)
      case NotRule(r) => !nullable(r) // TODO: check the semantics of this 
      case AnyRule => true
    }
  }
  
  def mkAndRule(r1:Rule, r2: Rule): Rule = {
    val r = (r1,r2) match {
      case (EmptyRule,e2) => e2
      case (e1,EmptyRule) => e1
      case (f@FailRule(_),_) => f
      case (_,f@FailRule(_)) => f
      case (_,_) => AndRule(r1,r2)
    }
    r
  } 
  
  def mkOrRule(r1:Rule, r2: Rule): Rule = {
    val r = (r1,r2) match {
      case (f@FailRule(_),e2) => e2
      case (e1,f@FailRule(_)) => e1
      case (e1,e2) => 
        if (e1 == e2) e1
        else OrRule(e1,e2)
    }
    r
  } 

   def mkRangeRule(m:Int,n:Int, r:Rule): Rule = {
    if (m < 0) FailRule("Range with negative lower bound = " + m)
    else if (m > n) FailRule("Range with lower bound " + m + " bigger than upper bound " + n)
    else {
     (m,n,r) match {
      case (0,0,_) => EmptyRule
      case (1,1,e) => e
      case (_,_,f@FailRule(_)) => f
      case (_,_,e@EmptyRule) => e
      case (m,n,e) => RangeRule(m,n,e)
     }
    }
  } 

   def mkRangeMinRule(m:Int, r:Rule): Rule = {
    if (m < 0) FailRule("Range with negative lower bound = " + m)
    else {
     (m,r) match {
      case (0,_) => EmptyRule
      case (1,e) => e
      case (_,f@FailRule(_)) => f
      case (_,e@EmptyRule) => e
      case (m,e) => RangeMinRule(m,e)
     }
    }
  }
   
  def deltaTriples(r:Rule, ts: Set[RDFTriple], ctx:Context): (Rule,Stream[Typing]) = {
    val e : Res = (r,Stream(ctx.typing))
    def f (b: Res, triple:RDFTriple): Res = {
      val (current,st1) = b
      log.debug("Calculating delta of " + showRule(current)(ctx.pm) + " with triple: " + triple )
      val (dr,st2) = delta(current,triple,ctx)
      log.debug(" step delta of rule: " + showRule(current)(ctx.pm) + " with triple: " + triple + " = " + showRule(dr)(ctx.pm))
      (dr,combineTypings(st1,st2))
    } 

    def combineTypings(st1: Stream[Typing], st2:Stream[Typing]): Stream[Typing] = {
      if (st1.isEmpty) st2
      else for (t1 <- st1; t2 <- st2) yield (t1 combine t2)
    }
    ts.foldLeft(e)(f)
  }


  def delta(rule: Rule, triple: RDFTriple, ctx: Context): 
	  	(Rule, Stream[Typing]) = {
    lazy val noTyping = Stream(ctx.typing)
    
    rule match {
      case ArcRule(_,n,v) =>
        if (matchName(ctx,triple.pred, n).isValid) {
          val mv = matchValue(ctx,triple.obj,v)
          if (mv.isValid) {
            (EmptyRule,mv.run)
          } else {
            (FailRule("Does not match value " + triple.obj + 
                " with ArcRule " + showRule(rule)(ctx.pm) + " Msg: " + mv.failMsg), 
             failTyping)
          }      
        } else {
          (FailRule("Does not match name " + triple.pred + 
              " with ArcRule " + showRule(rule)(ctx.pm)), 
              failTyping)
        }
      case RevArcRule(_,n,v) =>
        if (matchName(ctx,triple.pred, n).isValid) {
          val mv = matchValue(ctx,triple.subj,v)
          if (mv.isValid) {
            (EmptyRule,mv.run)
          } else {
            (FailRule("Does not match value " + triple.subj + 
                " with RevArcRule " + showRule(rule)(ctx.pm) + " Msg: " + mv.failMsg), 
             failTyping)
          }      
        } else {
          (FailRule("Does not match name " + triple.pred + 
              " with RevArcRule " + showRule(rule)(ctx.pm)), 
             failTyping)
        }

      case RelationRule(_,v1,v2) =>
          val mv1 = matchValue(ctx,triple.subj,v1)
          val mv2 = matchValue(ctx,triple.obj,v1)
          if (mv1.isValid) {
            if (mv2.isValid)
            (EmptyRule,mv1.run ++ mv2.run)
            else {
              (FailRule("Does not match value " + triple.obj + 
                " with RelationRule " + showRule(rule)(ctx.pm) + " Msg: " + mv2.failMsg), 
               failTyping)
            }
          } else {
            (FailRule("Does not match value " + triple.subj + 
                " with RelationRule " + showRule(rule)(ctx.pm) + " Msg: " + mv1.failMsg), 
             failTyping)
          }      

      case EmptyRule => 
        if (ctx.openShapes) (EmptyRule,noTyping)  
        else (FailRule("Unexpected triple " + triple),failTyping)
        			 
      case f@FailRule(msg) => {
        log.debug("...Failing rule " + showRule(rule)(ctx.pm) + " with " + msg)
        (f,failTyping)
      }
      case OrRule(r1,r2) => { 
        val (dr1,t1) = delta(r1,triple,ctx)
        val (dr2,t2) = delta(r2,triple,ctx)
        (mkOrRule(dr1,dr2),t1 ++ t2)
      }
      
      // The semantics of And is more close to interleave
      // TODO: check possible simplifications of this rule in case dr1 or dr2 are nullable
      case AndRule(r1,r2) => {
        val (dr1,t1) = delta(r1,triple,ctx)
        val (dr2,t2) = delta(r2,triple,ctx)
        (mkOrRule(mkAndRule(dr1,r2),mkAndRule(dr2,r1)), t1 ++ t2)
      }
      
      case e@StarRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        (mkAndRule(dr,e),t)
      }

      case OptRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        (dr,t)
      }

      case PlusRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        (mkAndRule(dr,StarRule(r)),t)
      }
      
      case RangeRule(m,n,r) => {
        val (dr,t) = delta(r,triple,ctx)
        (mkAndRule(dr,mkRangeRule(math.max(m - 1,0), n - 1, r)), t)
      }

      case RangeMinRule(m,r) => {
        val (dr,t) = delta(r,triple,ctx)
        (mkAndRule(dr,mkRangeMinRule(math.max(m - 1,0), r)), t)
      }
      
      case ActionRule(r,a) => delta(r,triple,ctx)

      case AnyRule => (EmptyRule,noTyping)

      case NotRule(r) => {
        val (dr,t) = delta(r,triple,ctx)
        dr match {
          case EmptyRule => 
            (FailRule("Not rule found triple " + t + " that matches " + showRule(rule)(ctx.pm)), noTyping)
          case FailRule(msg) => {
            (EmptyRule,noTyping)
          }
          case _ => (NotRule(dr),t)
        }
      }
        
    }
    
  }
  
  def showRule(rule: Rule)(implicit pm: PrefixMap): String = 
     ShapeDoc.rule2String(rule)(pm)

}

object ShapeValidatorWithDeriv {
  
}