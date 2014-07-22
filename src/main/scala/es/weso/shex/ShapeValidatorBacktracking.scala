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
 * Shape validator using Backtracking
 * It mimics the inference rule semantics but is computationally very expensive 
 */
trait ShapeValidatorBacktracking extends ShapeValidator with Logging {
  
override def id = "Validator by Backtracking 1.0"
  
override def matchRule (
    ctx: Context, 
    g: Set[RDFTriple], 
    rule: Rule ): Result[Typing] = {
  
  log.debug("matchRule " + showRule(rule))
  log.debug("Triples:")
  showTriples(g)
  rule match {
   
   case AndRule(r1,r2) => for(
       (g1,g2) <- parts(g)
     ; t1 <- matchRule(ctx,g1,r1)
     ; t2 <- matchRule(ctx,g2,r2)
     ) yield t1 combine t2
  

   case OrRule(r1,r2) => 
     matchRule(ctx,g,r1) orelse 
     matchRule(ctx,g,r2)
  
   case PlusRule(r) => {
     matchRule(ctx,g,r) orelse
     ( for (
         (g1,g2) <- parts(g)
       ; t1 <- matchRule(ctx,g1,r)
       ; t2 <- matchRule(ctx,g2,rule)
       ) yield t1 combine t2
     )
   }

   case StarRule(r) => {
     matchRule(ctx,g,EmptyRule) orelse matchRule(ctx,g,PlusRule(r))  
   }

   case OptRule(r) => {
     matchRule(ctx,g,r) orelse matchRule(ctx,g,EmptyRule)
   }

   case EmptyRule => 
    	if (g.isEmpty) unit(ctx.typing)
    	else {
    		val msg = "EmptyRule: graph non empty"
    		log.debug(msg)
    		failure(msg)
    	}

   case NotRule(r) => {
    if (matchRule(ctx,g,r).isFailure) unit(ctx.typing) 
    else {
      val msg = "NotRule: matches"
      log.debug(msg)
      failure(msg)
    }
   }

   case AnyRule => {
	 log.debug("matching with any")
     unit(ctx.typing)
   }

   case ArcRule(id,name,value) =>
    if (g.size == 1) {
      val triple = g.head
      for ( b <- matchName(ctx,triple.pred,name)
          ; typing <- matchValue(ctx,triple.obj,value)
          ) yield typing
    } else {
     val msg = "Arc expected but zero or more than one triple found in graph:\n" + g.toString
     log.debug("fail: " + msg)
     failure(msg)
    }

   case RevArcRule(id,name,value) =>
    if (g.size == 1) {
      val triple = g.head
      for ( b <- matchName(ctx,triple.pred,name)
          ; typing <- matchValue(ctx,triple.subj,value)
          ) yield typing
    } else {
      val msg = "RevArc expected one but zero or more than one triple found in graph:\n" + g.toString
      log.debug("fail: " + msg)
      failure(msg)
    } 

   case ActionRule(r,a) => {
     log.debug("Executing... " + a)
     unit(ctx.typing)
   }
   
   case FailRule(msg) => 
     failure(msg)
 
  }     

 }

}