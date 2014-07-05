package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.parser.PrefixMap
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeParser._
import scala.util.parsing.input.Positional
import scala.util.{Try, Success, Failure}
import es.weso.monads.Result
import es.weso.rdf.RDF
import org.slf4j._



/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 * 
 */

case class Schema(
    pm: PrefixMap, 
    shEx: ShEx) extends Positional {

  override def toString(): String = {
    ShapeDoc.schema2String(this)(pm)
  }
  
  def getLabels(): List[Label] = {
    shEx.rules.map(_.label).toList
  }

  def addAny: Schema = 
    Schema( pm.addPrefix("shex",ShapeSyntax.shex_IRI)
          , shEx.copy(rules = shEx.rules :+ ShapeSyntax.anyShape)
          )

}

object Schema extends ShapeValidator {

  def fromString(cs: CharSequence): Try[(Schema,PrefixMap)] = {
    ShapeParser.parse(cs) 
  }  

  def matchSchema( iri:IRI
		  		 , rdf:RDF
		  		 , schema: Schema
		  		 , validateIncoming: Boolean = false
		  		 , openShapes: Boolean = false
		  		 , withAny: Boolean = false
		  		 ): Result[Typing] = {

    val shex_extended = 
      if (withAny) schema.addAny.shEx
      else schema.shEx
    
    val ctx = 
      Context( rdf=rdf
    		 , shEx=shex_extended
    		 , Typing.emptyTyping 
    		 , validateIncoming
    		 , openShapes
    		 )

    def matchLabel(lbl: Label): Result[Typing] = {

      for ( shape <- ctx.getShape(lbl)
          ; ctx1 <- ctx.addTyping(iri,lbl.getNode)
          ; t <- matchShape(ctx1,iri,shape)
          ) yield t
    }
    
    Result.passSome(schema.getLabels,matchLabel)
  }
  
  def matchAll( rdf:RDF
		  	  , schema: Schema
		  	  , validateIncoming: Boolean = false
		  	  , openShapes: Boolean = false
		  	  , withAny: Boolean = false
		  	  ): Result[Typing] = {
    val subjects : List[IRI] = rdf.subjects.toList
    
    def eval(iri:IRI) = {
      matchSchema(iri,rdf,schema,validateIncoming,openShapes,withAny)
    }
    
    def comb(t1:Typing,t2:Typing):Typing = {
      t1 combine t2
    }
    Result.combineAll(subjects,eval,Typing.emptyTyping,comb)
  }

}
