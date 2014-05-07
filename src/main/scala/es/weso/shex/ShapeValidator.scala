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
import es.weso.rdf._
import es.weso.shex.Context._


object ShapeValidator {

def matchAll(ctx:Context): Result[Typing] = {

  def matchWithTyping(iri: IRI, typing: Typing)(shape: Shape): Result[Typing] = {
    for ( t <- matchShape(ctx,iri,shape)
        ) 
    yield typing.combine(t)
  }

  def matchSomeWithTyping(iri: IRI, typing: Typing): Result[Typing] = {
    Result.passSome(ctx.getShapes, matchWithTyping(iri, typing)) 
  }

  Result.passAll(ctx.getIRIs, emptyTyping, matchSomeWithTyping)

}



def matchShape(ctx:Context, iri: IRI, shape: Shape): Result[Typing] = {
 val triples = ctx.triplesWithSubject(iri)
 for (
   t <- matchRule(ctx,triples,shape.rule)
 ; newT <- Result.liftOption(t.addType(iri,shape.label.getIRI)) 
 ) yield newT 
} 

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
    matchRule(ctx,g,r2)
  
  case OneOrMore(r) => {
    matchRule(ctx,g,r) orelse
    ( for (
        (g1,g2) <- parts(g)
      ; t1 <- matchRule(ctx,g1,r)
      ; t2 <- matchRule(ctx,g2,rule)
      ) yield t1 combine t2
    )
  }

  case NoRule => 
    if (g.isEmpty) unit(emptyTyping)
    else failure("EmptyRule: graph non empty")

  case ActionRule(r,a) => failure("Action not implemented yet")
  case ArcRule(id,n,v) =>
    if (g.size == 1) {
      val t = g.head
      for ( b <- matchName(ctx,t.pred,n)
          ; t <- matchValue(ctx,t.obj,v)
          ) yield t
    } else 
       failure("Arc expected but more zero or more than one triple found in graph")

 }
 

 def matchName(ctx: Context, pred: IRI, n: NameClass): Result[Boolean] =
   n match {
   case NameTerm(t) => {
     if (pred == t) unit(true)
     else failure("matchName: iri=" + pred + " does not match name=" + t)
   }
   case NameAny(excl) => {
     if (matchStems(excl, pred)) failure("matchName: iri= " + pred + " appears in excl= " + excl)
     else unit(true)
   }
   case NameStem(s) => {
     if (s.matchStem(pred)) unit(true) 
     else failure("matchName: iri= " + pred + " does not match stem= " + s)
   } 
 }
   
   
 def matchValue(ctx: Context, obj: RDFNode, v: ValueClass): Result[Typing] = { 
  v match {
    case ValueType(v) => 
      for ( b <- matchType(obj,v); if (b)) yield emptyTyping
    case ValueSet(s) => 
      if (s contains(obj)) unit(emptyTyping)
      else failure("matchValue: obj" + obj + " is not in set " + s)
    case ValueAny(stem) => ???
    case ValueStem(s) => ???
    case ValueReference(l) => 
      if (obj.isIRI) {
      for (
        shape <- ctx.getShape(l)
      ; newT <- matchShape(ctx,obj.toIRI,shape)
      ) yield newT
      }
      else failure("ValueReference: object " + obj + " must be an IRI")
  }
 }
   
 def matchType(obj: RDFNode, vtype: RDFNode): Result[Boolean] = {
   obj match {
     case lit:Literal => if (lit.dataType == vtype) unit(true)
                         else unit(false)
     case _ => failure ("matchType: obj " + obj + " must is not a literal")
   }
 }
 
/* def emptyContext : Context = 
   Context(RDFTriples.noTriples,
   ) */
}