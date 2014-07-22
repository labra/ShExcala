package es.weso.shex

import es.weso.monads.Result
import es.weso.rdf.RDF
import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._

case class Matcher(schema: Schema
    , rdf: RDF
    , validateIncoming: Boolean = false
	, withAny: Boolean = false
	) {

  val subjects : List[IRI] = rdf.subjects.toList
  
  val shex_extended = 
      if (withAny) schema.addAny.shEx
      else schema.shEx
    
    val ctx = 
      Context( rdf=rdf
    		 , shEx=shex_extended
    		 , Typing.emptyTyping
    		 , schema.pm
    		 , validateIncoming
    		 )

    def matchIRI_Label(iri: IRI)(lbl: Label): Result[Typing] = {
      for ( shape <- ctx.getShape(lbl)
          ; ctx1 <- ctx.addTyping(iri,lbl.getNode)
          ; t <- Schema.matchShape(ctx1,iri,shape)
          ) yield t
    }
    
    def matchLabel_IRI(lbl: Label)(iri: IRI): Result[Typing] = {
      matchIRI_Label(iri)(lbl)
    }

    def matchIRI_AllLabels(iri: IRI): Result[Typing] = {
      Result.passSome(schema.getLabels,matchIRI_Label(iri))
    }

    def comb(t1:Typing,t2:Typing):Typing = {
      t1 combine t2
    }

    def matchAllIRIs_Label(lbl: Label): Result[Typing] = {
      Result.combineAll(subjects,matchLabel_IRI(lbl),comb)
    }

    def matchAllIRIs_AllLabels(): Result[Typing] = {
      Result.combineAll(subjects,matchIRI_AllLabels,comb)
    }
}