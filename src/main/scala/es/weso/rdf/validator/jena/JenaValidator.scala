package es.weso.rdf.validator.jena

import com.hp.hpl.jena.rdf.model.Model
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl._
import es.weso.rdf.validator._
import es.weso.rdf.nodes._
import util._

object JenaValidator {
  def mkValidator(
      schemaStr: String, 
      schemaFormat: String = "SHEX", 
      model: Model): Try[RDFValidator] = {
    val rdf = RDFAsJenaModel(model)
    for {
      (schema,pm) <- Schema.fromFile(schemaStr, schemaFormat, None) 
    } yield ShaclMatcher(schema,rdf)
  }
    
}
