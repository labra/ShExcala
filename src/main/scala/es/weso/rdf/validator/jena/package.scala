package es.weso.rdf.validator

import com.hp.hpl.jena.rdf.model.Model
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl._
import es.weso.rdfgraph.nodes._
import io._

package object jena {
  type Result = ValidationResult[RDFNode,Label,Throwable]
  val format = SchemaFormats.default
  def validate(schemaStr: String, model: Model): Try[Result] = {
    for {
      schema <- Schema.fromFile(schemaStr,format,None)
      rdf <- RDFAsJenaModel(model)
      results <- ShaclMatcher(schema,rdf).matchAllNodes_AllLabels
    } yield results
  }
}