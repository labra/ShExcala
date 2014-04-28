package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
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
    g: Set[RDFTriple], 
    rule: Rule ): Result[Typing] = 
 rule match {

  case AndRule(r1,r2) => for(
      (g1,g2) <- parts(g)
    ; t1 <- matchRule(ctx,g1,r1)
    ; t2 <- matchRule(ctx,g2,r2)
    ) yield t1 combine t2
  

  case OrRule(r1,r2) => 
    matchRule(ctx,g,r1) orelse 
    matchRule(ctx,g,r1)
  
  case OneOrMore(r) => {
    matchRule(ctx,g,r) orelse
    ( for (
        (g1,g2) <- parts(g)
      ; t1 <- matchRule(ctx,g1,r)
      ; t2 <- matchRule(ctx,g2,r)
      ) yield t1 combine t2
    )
  }

  case NoRule => if (g.isEmpty) unit(emptyTyping)
  else failure("EmptyRule: graph non empty")

  case ActionRule(r,a) => failure("Action not implemented yet")
  case ArcRule(id,n,v) => failure("Arc not implemented yet")

  case _ => failure("Not implemented yet")

 }
 
}