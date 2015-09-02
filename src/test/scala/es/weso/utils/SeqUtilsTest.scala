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
  
  describe("zipN with looooong lists") {
    val looongStream = (1 to 1000000000).toStream
    it(s"Should be able to zip a stream of loong elements and take 1 value") {
      zipN(Stream(Stream(1,2),Stream(),looongStream)).take(1) should be(Stream(Stream(Some(1),None,Some(1))))
    }
    it(s"Should be able to zip a stream of loong elements and take 2 values") {
      zipN(Stream(Stream(1,2),Stream(),looongStream)).take(2) should be(
          Stream(Stream(Some(1),None,Some(1)),
                 Stream(Some(1),None,Some(2))))
    }
  }
  
  def testZipN[A](vs: List[List[A]], expected: List[List[A]]) = {
   it(s"zipN($vs) should be $expected") {
    zipN(vs) should be(expected) 
   }
  }

}