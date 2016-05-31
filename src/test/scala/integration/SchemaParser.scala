package integration

import org.scalatest._
import org.scalatest.prop._
import es.weso.shex._
import ShExSchema._
import Label._
import es.weso.rdf.nodes._
import util._
import es.weso.rdf.PrefixMap


class SchemaParser 
    extends FunSpec
    with Matchers
    with Checkers 
    with TryValues {

  describe("Shacl Parser") {

      it("Should parse a schema") {
/*        val str = """|$<http://a.example/vc1> = LITERAL pattern "^ab"
                     |$<http://a.example/vc2> = LITERAL pattern "cd"
                     |$<http://a.example/vc3> = LITERAL pattern "ef$"
                     |$<http://a.example/vc4> = $<http://a.example/vc1> OR $<http://a.example/vc2> OR $<http://a.example/vc3>
                     |<http://a.example/S1> {
                     |   <http://a.example/p1> $<http://a.example/vc4>
                     |}""".stripMargin */
        
        val str = """|<S1> { 
                     | <p> LITERAL 
                     |}""".stripMargin

        info(s"str: $str")                     
        val result = Schema.fromString(str)
        
        val shape : Shape = BasicShape.empty.copy(
            shapeExpr = TripleConstraint.empty.copy(
                iri = IRI("p"),
                value = LiteralKind(List())
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

