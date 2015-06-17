package es.weso.shacl

import es.weso.shacl.Shacl._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import es.weso.monads.Result._
import es.weso.monads._
import util._
import es.weso.rdf.jena.RDFAsJenaModel

/**
 * Utility test to isolate one test
 */
class SingleSpec
    extends FunSpec
    with ShaclValidator
    with Matchers
    with Checkers {

  describe("Single") {
    it("Should match node with label (single)") {
      val rdf_str = """|@prefix : <http://example.org/>. 
                       |:x  :b 1 .""".stripMargin
      val rdf = RDFAsJenaModel.fromChars(rdf_str,"TURTLE").get
      val ex = IRI("http://example.org/")
      val node = ex.add("x")
      
      val shape_str = """|prefix : <http://example.org/>
                         |:shape {
                         |   :b (1)?
                         |   }""".stripMargin
      val (schema,pm) = Schema.fromString(shape_str).get
      val label = IRILabel(ex.add("shape"))
      val ctx = Context(rdf,schema.shaclSchema,Typing.emptyTyping,pm,true)
      matchNodeLabel_shouldPass(node,label,ctx,false)
  }
  }
    
  def matchNodeLabel_shouldPass(
      node: RDFNode, 
      label: Label,
      ctx: Context,
      withTrace: Boolean = false): Unit = {
    val result: Result[ValidationState] = for {
      _ <- setTrace(withTrace)
      r <- matchNodeLabel(node,label,ctx)
    } yield r
    
    if (result.isValid) {
      val sol1 = result.run.get
      val condition = sol1.filter(_.containsNodeLabel(node,label)).size > 0 
      if (!condition) {
        info("Result doesn't contain type " + node + "/" + label)
        info("Result: " + sol1.toList)
      }
      condition should be(true)
    } else {
      info("matchingNodeLabel: " + node + " with " + label)
      fail("Result: " + result + " is not valid")
    }
  }
  
}

