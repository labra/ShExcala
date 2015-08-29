package es.weso.utils

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import SeqUtils._

class SeqUtilsTest extends FunSpec with Matchers with Checkers {

  describe("zipN") {
    testZipN(List(List(1,2)),
        List(List(Some(1)),List(Some(2))))
    testZipN(List(List(1,2),List(3,4)),
        List(List(Some(1),Some(3)),List(Some(1),Some(4)),List(Some(2),Some(3)),List(Some(2),Some(4))))
    testZipN(List(List(1,2),List(3,4,5)),
        List(List(Some(1),Some(3)),List(Some(1),Some(4)),List(Some(1),Some(5)),List(Some(2),Some(3)),List(Some(2),Some(4)), List(Some(2),Some(5))))
    testZipN(List(List(0),List(1,2),List()),
        List(List(Some(0),Some(1),None), 
             List(Some(0),Some(2),None)))
    testZipN(List(List(0),List(),List(1,2),List(),List(3)),
        List(List(Some(0),None,Some(1),None,Some(3)), 
             List(Some(0),None,Some(2),None,Some(3))))
  }
  
  def testZipN[A](vs: List[List[A]], expected: List[List[A]]) = {
   it(s"zipN($vs) should be $expected") {
    zipN(vs) should be(expected) 
   }
  }

}