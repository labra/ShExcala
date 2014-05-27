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

case class Schema(pm: PrefixMap, shEx: ShEx) extends Positional {

  override def toString(): String = {
    val sd = ShapeDoc(pm)
    sd.schema2String(this)
  }
  
  def getLabels(): List[Label] = {
    shEx.rules.map(_.label).toList
  }

}

object Schema {

  val log = LoggerFactory.getLogger("Schema")
  
  def fromString(cs: CharSequence): Try[(Schema,PrefixMap)] = {
    ShapeParser.parse(cs) 
  }  

  def matchSchema(iri:IRI, rdf:RDF, schema: Schema, validateIncoming: Boolean = false): Result[Typing] = {
    
    val ctx = Context(rdf=rdf,shEx=schema.shEx, Typing.emptyTyping, validateIncoming)

    def matchLabel(lbl: Label): Result[Typing] = {

      for ( shape <- ctx.getShape(lbl)
          ; ctx1 <- ctx.addTyping(iri,lbl.getIRI)
          ; t <- matchShape(ctx1,iri,shape)
          ) yield t
    }
    
    Result.passSome(schema.getLabels,matchLabel)
  }
}
