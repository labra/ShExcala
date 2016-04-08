package es.weso.shex

import org.scalatest._

import es.weso.shex.SchemaFormat;

class SchemaFormatsTest extends FunSpec with Matchers with TryValues {
  
  describe("Get SchemaFormats") {

    it("should get schema format") {
      SchemaFormat.available("TURTLE") should be(true)
      SchemaFormat.available("turtle") should be(true)
      SchemaFormat.available("foo") should be(false)
    }
  }

}