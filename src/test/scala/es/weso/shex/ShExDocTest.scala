package es.weso.shex

import org.scalatest._
import util._

class ShExDocTest extends FunSpec with Matchers with TryValues {
  
  describe("ShaclDoc") {

    it("should be able to parse a serialized Schema") {
      val str = """|prefix : <http://example.org>
                   |
                   |:S { :p [ 1 ] }""".stripMargin
      val tryConv = for {
        (schema,pm) <- Schema.fromString(str,"SHEXC")
        val strSerialized = schema.serialize("SHEXC")
        (schema2,pm) <- {
          info(s"Trying to parse: $strSerialized")
          Schema.fromString(strSerialized,"SHEXC")
        }
      } yield (schema,schema2)
      
      tryConv match {
        case Failure(e) => fail(s"Failure: $e")
        case Success((schema1,schema2)) => schemasShouldBeEqual(schema1,schema2) 
      }
    }
  }
  
  def schemasShouldBeEqual(schema1: Schema, schema2: Schema): Unit = {
    if (schema1 != schema2) {
      fail(s"schemas should be equal\nSchema1: " + 
          schema1.serialize("SHEXC") + 
          "\nschema2: " + schema2.serialize("SHEXC"))
    }
  }

}