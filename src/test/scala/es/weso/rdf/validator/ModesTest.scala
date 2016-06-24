package es.weso.rdf.validator

import org.rogach.scallop.Scallop
import org.scalatest._

class ModesTest
    extends FunSpec
    with Matchers 
    with OptionValues {

  describe("Modes") {

    it("lookup mode") {
      println("modes " + Modes.modes)
      Modes.lookup("s") should be(Some(Modes.allNodes_start))
      Modes.lookup("Declared") should be(Some(Modes.declared))
      Modes.lookup("D") should be(Some(Modes.declared))
    }
  }

}