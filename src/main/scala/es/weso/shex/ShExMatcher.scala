package es.weso.shex

import es.weso.monads._
import es.weso.rdf._
import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import java.lang._
import es.weso.utils.Logging
import es.weso.rdf.validator._

case class ShExMatcher(
    schema: Schema, 
    rdf: RDFReader, 
    validateIncoming: Boolean = false, 
    withAny: Boolean = false, 
    validator: ShapeValidator = ShapeValidatorWithDeriv
   ) extends Logging 
      with   RDFValidator {
  
  type Label = ShapeSyntax.Label
  type Schema = es.weso.shex.Schema
  override def id = "ShEx (Labra) 1.0"
  override def labels: Seq[Label] = schema.getLabels
  override def match_node_label(node: RDFNode)(label: Label): ShExResult = {
    node match {
      case iri: IRI => {
        ShExResult(matchIRI_Label(iri)(label)) 
      }
      case _ => throw new Exception(s"Node must be a IRI to be validated: $node. Label: $label")
    }
  }
  
  implicit def result(r: Result[Typing]): ValidationResult[RDFNode,Label] = ???

  override def subjects: List[RDFNode] = rdf.subjects.toList

  val shex_extended =
    if (withAny) schema.addAny.shEx
    else schema.shEx

  val ctx =
    Context(rdf = rdf, shEx = shex_extended, Typing.emptyTyping, schema.pm, validateIncoming
    )

  def matchIRI_Label(iri: RDFNode)(lbl: Label): Result[Typing] = {
    log.debug("Matching " + iri + " with label " + lbl)
    try {
      for (
        shape <- ctx.getShape(lbl); 
        ctx1 <- ctx.addTyping(iri, lbl.getNode); 
        t <- validator.matchShape(ctx1, iri, shape)
      ) yield {
        log.debug("Matched with typing: " + t)
        t
      }
    } catch {
      case _: StackOverflowError => Failure("StackOverflow error")
      case e: Exception => Failure("Exception matching iri " + iri + " with label " + lbl + ": " + e.getMessage)
    }
  }

  def matchLabel_IRI(lbl: Label)(iri: RDFNode): Result[Typing] = {
    matchIRI_Label(iri)(lbl)
  }

  def matchIRI_AllLabels(iri: RDFNode): Result[Typing] = {
    Result.passSome(schema.getLabels, matchIRI_Label(iri))
  }

  def comb(t1: Typing, t2: Typing): Typing = {
    t1 combine t2
  }

  def matchAllIRIs_Label(lbl: Label): Result[Typing] = {
    Result.combineAll(subjects, matchLabel_IRI(lbl), comb)
  }

  def matchAllIRIs_AllLabels(): Result[Typing] = {
    Result.combineAll(subjects, matchIRI_AllLabels, comb)
  }
}