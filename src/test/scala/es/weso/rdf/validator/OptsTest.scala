package es.weso.rdf.validator

import es.weso.rdf.validator._
import org.scalatest._
import org.rogach.scallop.Scallop

class OptsTest
    extends FunSpec
    with Matchers 
    with OptionValues {

  describe("OptsTest") {

    it("parse simple opts") {
      val errorDriver = (e: Throwable, scallop: Scallop) => {
        fail("Error: " + e)
      }
      val strOpts = "-h -s schema.shex"
      val args = strOpts.trim.split("\\s+")
      val opts = new Opts(args,errorDriver)
      opts.verify()
      val schema = opts.schema()
      schema should be("schema.shex")
      opts.help() should be(true)
      opts.verbose() should be(false)
    }
  }

}