package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._

import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.parser.PrefixMap
import scala.util.parsing.input.Positional

case class Context() {
  
}

object ShapeValidator {
  
def combineTypings(x:Typing,y:Typing):Typing = ???

def matchRule (
    ctx: Context, 
    triples: Set[RDFTriple], 
    rule: Rule ): Result[Typing] = 
 rule match {

  case AndRule(r1,r2) => for(
      t1 <- matchRule(ctx,triples,r1)
    ; t2 <- matchRule(ctx,triples,r2)
    ) yield combineTypings(t1,t2)
  

  case OrRule(r1,r2) => 
    matchRule(ctx,triples,r1) orelse matchRule(ctx,triples,r1)
  
  case ArcRule(id,n,v,c,a) => failure("Arc not implemented yet")

  case _ => failure("Not implemented yet")

 }
 
}