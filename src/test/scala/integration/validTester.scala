package integration

import es.weso.shacl.Schema
import es.weso.shacl.ShaclMatcher
import es.weso.rdf.jena.RDFAsJenaModel
import util._
import es.weso.rdf.nodes._
import es.weso.shacl.Label._
import es.weso.rdf.validator._
import org.scalatest._

trait ValidTester extends FunSpec with Matchers {
   def shouldBeValidAllNodes(strSchema: String, strData: String): Unit = {
    val result = for {
      (schema, pm) <- Schema.fromString(strSchema)
      data <- RDFAsJenaModel.fromChars(strData, "TURTLE")
      val validator : RDFValidator = ShaclMatcher(schema, data)
      val ts = validator.matchAllNodes_AllLabels
    } yield ts

    result match {
      case Failure(e) => fail(s"Exception: $e")
      case Success(ts) =>
        if (ts.isFailure) fail(s"No results: $ts")
        else info(s"Validated with typings $ts")
    }
  }
def shouldBeValid(
    strSchema: String, 
    strData: String, 
    strNode: String, 
    strLabel: String,
    verbose: Boolean = false): Unit = {
    val result = for {
      (schema,pm) <- Schema.fromString(strSchema,"SHEXC",Some(""))
      data <- RDFAsJenaModel.fromChars(strData,"TURTLE",Some(""))
      ts <- schema.matchNode_Label(IRI(strNode),labelStr(strLabel),data)
    } yield ts
    
    result match {
      case Failure(e) => {
       e.printStackTrace()
       fail(s"Exception: ${e.getStackTrace}") 
      }
      case Success(ts) => 
        if (ts.isEmpty) 
          fail("No results")
        else 
          info(s"Validated with typings $ts")
    }
  }
    
  def shouldNotBeValid(strSchema: String, strData: String, strNode: String, strLabel: String): Unit = {
    val result = for {
      (schema,pm) <- Schema.fromString(strSchema)
      data <- RDFAsJenaModel.fromChars(strData,"TURTLE")
      ts <- schema.matchNode_Label(IRI(strNode),labelStr(strLabel),data)
    } yield ts
    
    result match {
      case Success(ts) if (!ts.isEmpty) => fail(s"Validates with $ts but should fail")
      case _ => return
    }
     
  }
}