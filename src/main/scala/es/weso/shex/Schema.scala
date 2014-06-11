package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.parser.PrefixMap
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeParser._
import es.weso.shex.ShapeValidator._
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
    val sd = ShapeDoc(pm)
    sd.schema2String(this)
  }
  
  def getLabels(): List[Label] = {
    shEx.rules.map(_.label).toList
  }

  def addAny: Schema = 
    Schema( pm.addPrefix("shex",ShapeSyntax.shex_IRI)
          , shEx.copy(rules = shEx.rules :+ ShapeSyntax.anyShape)
          )

}

object Schema {

  val log = LoggerFactory.getLogger("Schema")
  
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
  
  def matchAll(rdf:RDF
		  	  , schema: Schema
		  	  , validateIncoming: Boolean = false
		  	  , openShapes: Boolean = false
		  	  , withAny: Boolean = false
		  	  ): Result[Typing] = {
      def step(iri:IRI,t:Typing):Result[Typing] = {
        for ( t1 <- matchSchema(iri,rdf,schema,validateIncoming,openShapes,withAny)
            ) yield t1 combine t
      }
      
      val subjects : List[IRI] = rdf.subjects.toList
      Result.passAll(subjects,Typing.emptyTyping,step)
  }

}
