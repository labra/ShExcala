package es.weso.rdf.validator.jena

import com.hp.hpl.jena.rdf.model.Model
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl._
import es.weso.rdf.validator._
import es.weso.rdf.nodes._
import util._

/**
 * JenaValidator. Validator based on Jena
 * 
 */
object JenaValidator {
  
  /**
   * Creates an RDFValidator from an Jena model
   * 
   * @param schemaStr Schema 
   * @param schemaFormat Format of the Schema (e.g. SHEXC)
   * @model Jena [[com.hp.hpl.jena.rdf.model.Model]]
   */
  def mkValidator(
      schemaStr: String, 
      schemaFormat: String = "ShExC", 
      model: Model): Try[RDFValidator] = {
    val rdf = RDFAsJenaModel(model)
    for {
      (schema,pm) <- Schema.fromFile(schemaStr, schemaFormat, None) 
    } yield ShaclMatcher(schema,rdf)
  }
    
}
