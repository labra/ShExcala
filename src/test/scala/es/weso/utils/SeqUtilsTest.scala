package es.weso.utils

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import SeqUtils._

class SeqUtilsTest extends FunSpec with Matchers with Checkers {

  describe("zipN") {
    testZipN(List(List(1,2)),List(List(1),List(2)))
    testZipN(List(List(1,2),List(3,4)),List(List(1,3),List(1,4),List(2,3),List(2,4)))
    testZipN(List(List(1,2),List(3,4,5)),List(List(1,3),List(1,4),List(1,5),List(2,3),List(2,4), List(2,5)))
  }
  
  def testZipN[A](vs: List[List[A]], expected: List[List[A]]) = {
   it(s"zipN($vs) should be $expected") {
    zipN(vs) should be(expected) 
   }
  }

}