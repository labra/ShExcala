package es.weso.rdf.validator.jena

import com.hp.hpl.jena.rdf.model.Model
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl._
import es.weso.rdf.validator._

import es.weso.rdfgraph.nodes._
import io._

object JenaValidator {
  type Result = ValidationResult[RDFNode,Label,Throwable]
  val format = SchemaFormats.default
  
  def validate(schemaStr: String, model: Model): Result = {
    val rdf = new RDFAsJenaModel(model)
    for {
      (schema,pm) <- Schema.fromFile(schemaStr,format,None)
      val validator : RDFValidator = ShaclMatcher(schema,rdf)
      r <- validator.matchAllNodes_AllLabels
    } yield r
  }

}