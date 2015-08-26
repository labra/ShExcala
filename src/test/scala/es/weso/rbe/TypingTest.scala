package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.Interval._
import util.{Try,Failure,Success}

class TypingTest extends FunSpec with Matchers with TryValues {

  describe("Typings") {
    
     val t : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3")))
    
     it("should add a typing to an element that doesn't exist") {
      t.addPosType("n3","t2") should be(Success(PosNegTyping.fromPosMap(
          Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3"), "n3" -> Set("t2"))
          )))
    }
    
     it("should add a negative typing to an element that doesn't exist") {
      t.addNegType("n3","t2") should be(Success(PosNegTyping.fromPosNegMap(
          Map("n1" -> (Set("t1","t2"),Set[String]()), 
              "n2" -> (Set("t1","t3"),Set[String]()), 
              "n3" -> (Set[String](),Set("t2")))
          )))
    }
     
    it("should not add a negative typing to an element that exist with positive typing") {
      t.addNegType("n2","t3").failure 
    }
  
    it("should add a negative typing to an element that exist with a different positive typing") {
      t.addNegType("n2","t4").success 
    }
  }
}