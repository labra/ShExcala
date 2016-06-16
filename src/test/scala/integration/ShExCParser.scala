package integration

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.shex.Label._
import es.weso.shex._
import org.scalatest._
import org.scalatest.prop._

import scala.util._


class ShExCParser
    extends FunSpec
    with Matchers
    with Checkers 
    with TryValues {

  describe("ShExC Parser") {

      it("Should parse a schema") {

        val str = """|<S1> { 
                     | <p> .
                     | %<http://shex.io/extensions/Test/>{ print("group semAct 1") %}
                     |}""".stripMargin

        info(s"str: $str")                     
        val result = Schema.fromString(str)
        
        val shape : Shape = BasicShape.empty.copy(
            shapeExpr = TripleConstraint.empty.copy(
                iri = IRI("p"),
                value = ValueClass.any,
                actions = Actions(List(Action(
                 name=IRI("http://shex.io/extensions/Test/"),
                 contents =" print(\"group semAct 1\") ")))
                )
            )
        val expected = 
          Schema.empty.copy(shexSchema = 
            ShExSchema.empty.copy(
                shapes = Map(labelStr("S1") -> shape)))
            
        result.success should be(Success((expected,PrefixMap.empty)))
      }
  }

}

