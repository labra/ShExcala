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
import org.slf4j._
import scala.util.matching.Regex


object ShapeValidator {

 val log = LoggerFactory.getLogger("ShapeValidator")

 def matchAll(ctx:Context): Result[Typing] = {

  def matchWithTyping(iri: IRI, typing: Typing)(shape: Shape): Result[Typing] = {
    println("matchSome. iri: " + iri.toString + 
            "\n--typing: " + typing.toString + 
            "\n--shape: " + shape.toString)
    for ( t <- matchShape(ctx,iri,shape)) 
    yield typing.combine(t)
  }

  def matchSomeWithTyping(iri: IRI, typing: Typing): Result[Typing] = {
    println("matchSome. iri: " + iri.toString + ". typing: " + typing.toString)
    Result.passSome(ctx.getShapes, matchWithTyping(iri, typing)) 
  }

  println("ctx: " + ctx.toString)
  println("iris: " + ctx.getIRIs)
  Result.passAll(ctx.getIRIs, emptyTyping, matchSomeWithTyping)

}



def matchShape(ctx:Context, node: RDFNode, shape: Shape): Result[Typing] = {

 val triples = ctx.triplesAround(node)
 for (
   ctx1 <- ctx.addTyping(node,shape.label.getNode)
 ; t <- matchRule(ctx1,triples,shape.rule)
 ) yield t 
} 

def matchRule (
    ctx: Context, 
    g: Set[RDFTriple], 
    rule: Rule ): Result[Typing] = {
  
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
    if (g.isEmpty) unit(ctx.typing)
    else failure("EmptyRule: graph non empty")

   case NotRule(r) => {
    if (matchRule(ctx,g,r).isFailure) unit(ctx.typing) 
    else failure("NotRule: matches") 
   }

   case RevArcRule(id,name,value) =>
    if (g.size == 1) {
      val triple = g.head
      for ( b <- matchName(ctx,triple.pred,name)
          ; typing <- matchValue(ctx,triple.subj,value)
          ) yield typing
    } else 
       failure("RevArc expected one but zero or more than one triple found in graph:\n" + g.toString)

   case ArcRule(id,name,value) =>
    if (g.size == 1) {
      val triple = g.head
      for ( b <- matchName(ctx,triple.pred,name)
          ; typing <- matchValue(ctx,triple.obj,value)
          ) yield typing
    } else 
       failure("Arc expected but zero or more than one triple found in graph:\n" + g.toString)

   case ActionRule(r,a) => failure("Action not implemented yet")
 
  }     

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
      for ( b <- matchType(obj,v) 
          ; if (b)
          ) 
        yield ctx.typing
          
    case ValueSet(s) => 
      if (Result.passSome(s.toList, 
    		  			  matchValueObject(obj)).isValid) 
        unit(ctx.typing)
      else failure("matchValue: obj" + obj + " is not in set " + s)

    case ValueAny(excl) => {
      if (matchStems(excl, obj)) failure("matchValue, value any: iri= " + obj + " appears in excl= " + excl)
      else unit(ctx.typing)
    }

    case ValueStem(s) => {
      if (s.matchStem(obj)) unit(ctx.typing)
      else failure("matchValue, value stem: iri = " + obj + " does not have stem = " + s)
    }

    case ValueReference(l) => 
      if (obj.isIRI) {
        if (ctx.containsType(obj.toIRI,l.getNode)) unit(ctx.typing)
        else 
       	for ( shape <- ctx.getShape(l)
       		; newT <- matchShape(ctx,obj.toIRI,shape)
       		) yield newT
      }
      else failure("ValueReference: object " + obj + " must be an IRI")
  }
 }
   
 def matchType(obj: RDFNode, vtype: RDFNode): Result[Boolean] = {
   
   obj match {
     case lit:Literal => {
       if (vtype == shex_Literal || 
           vtype == shex_NonIRI ||
           vtype == shex_NonBNode ||
           lit.dataType == vtype) 
         unit(true)
       else unit(false)
     }                         
     case iri:IRI => {
       if (vtype == shex_IRI || 
           vtype == shex_NonLiteral ||
           vtype == shex_NonBNode) unit(true)
       else unit(false)
     }				 
     case bnode:BNodeId => 
       if (vtype == shex_BNode ||
           vtype == shex_NonIRI ||
           vtype == shex_NonLiteral) unit(true)
       else unit(false)
   }

 }
 
 def matchValueObject(node: RDFNode)(vo: ValueObject) : Result[Boolean] = {
   vo match {
     case RDFNodeObject(n) => unit(n == node)
     case LangObject(lang) => node match {
       case l:LangLiteral => matchLang(lang,l)
       case _ => failure("matchVallueObject: value object: " + vo + " does not match " + node)
     }
     case RegexObject(r,None) => 
       node match {
         case l:StringLiteral => matchRegex(r,l)
         case _ => failure("matchValueObject: Regex " + r + " does not match node " + node) 
       }
     case RegexObject(r,Some(lang)) => 
       node match {
         case l:LangLiteral => 
           for ( _ <- matchRegex(r,l)
               ; b <- matchLang(lang,l)
         	   ) yield b
         case _ => failure("matchValueObject: Regex " + r + " with lang " + lang + " does not match node " + node)
         }
   }
   
 }

 def matchRegex(r: Regex, lit: Literal): Result[Boolean] = {
   r.findFirstIn(lit.lexicalForm) match {
     case None => failure("matchValue: regex " + r.toString + " does not match literal " + lit.lexicalForm)
     case Some(_) => unit(true)
   }
 }
 
 def matchLang(lang:Lang, lit: LangLiteral): Result[Boolean] = {
   // TODO: Improve language matching
   if (lang == lit.lang)
         unit(true)
   else failure("Lang " + lang + " does not match lang of literal " + lit)
 }

}