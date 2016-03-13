package es.weso.rdf.validator.jena

import org.scalatest._
import util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.nodes.IRI
import es.weso.shacl.Label
import es.weso.rdf.parser._
import es.weso.utils.JenaUtils
import es.weso.utils.Parsed
import es.weso.utils.NotParsed


class JenaValidatorTest extends FunSpec with Matchers with TryValues {

  describe("JenaValidator") {

    it("Validates a simple example") {

      val strRdf = """|@prefix : <http://example.org/>
                      |
                      |:x :p 1 .
                      |""".stripMargin

      val parseReport = JenaUtils.str2Model(strRdf)
      parseReport match {
        case Parsed(model) => {
          val fileSchema = getClass.getResource("/schemas/simpleSchema.shex").getFile
          val tryValidator = JenaValidator.mkValidator(fileSchema, "ShExC", model)
          tryValidator match {
            case Success(validator) => {
              val node: RDFNode = IRI("http://example.org/x")
              val label = validator.labelStr("http://example.org/S")
              val result = validator.match_node_label(node)(label)
              result.isValid should be(true)
            }
            case Failure(e) => fail("Error making validator " + e)
          }

        }
        case NotParsed(str) => fail("Error parsing: " + str)
      }

    }
  }
}
