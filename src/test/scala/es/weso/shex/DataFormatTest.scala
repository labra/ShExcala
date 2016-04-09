package es.weso.shex

import org.scalatest._

class DataFormatTest extends FunSpec with Matchers with TryValues {
  
  describe("Get SchemaFormats") {

    it("should get data format") {
      DataFormat.available("TURTLE") should be(true)
      DataFormat.available("turtle") should be(true)
      DataFormat.available("foo") should be(false)
    }
    
    it("should lookup data format") {
      DataFormat.lookup("TURTLE").success.value should be(DataFormat.TURTLE)
      DataFormat.lookup("turtle").success.value should be(DataFormat.TURTLE)
      DataFormat.lookup("foo").failure
    }

    it("should get default data format") {
      DataFormat.default should be(DataFormat.TURTLE)
    }
    
  }

}