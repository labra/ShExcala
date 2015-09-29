package es.weso.utils

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import SeqUtils._

class SeqUtilsTest extends FunSpec with Matchers with Checkers {

    describe("zipNOption") {
    testZipN(List(List(1,2)),
        List(List(1),List(2)))
    testZipN(List(List(1,2),List(3,4)),
        List(List(1,3),List(1,4),List(2,3),List(2,4)))
    testZipN(List(List(1,2),List(3,4,5)),
        List(List(1,3),List(1,4),List(1,5),List(2,3),List(2,4), List(2,5)))
    testZipN(List(List(0),List(1,2),List()),
        List(List(0,1), 
             List(0,2)))
    testZipN(List(List(0),List(),List(1,2),List(),List(3)),
        List(List(0,1,3), 
             List(0,2,3)))
  }

  describe("zipNOption") {
    testZipNOpt(List(List(1,2)),
        List(List(Some(1)),List(Some(2))))
    testZipNOpt(List(List(1,2),List(3,4)),
        List(List(Some(1),Some(3)),List(Some(1),Some(4)),List(Some(2),Some(3)),List(Some(2),Some(4))))
    testZipNOpt(List(List(1,2),List(3,4,5)),
        List(List(Some(1),Some(3)),List(Some(1),Some(4)),List(Some(1),Some(5)),List(Some(2),Some(3)),List(Some(2),Some(4)), List(Some(2),Some(5))))
    testZipNOpt(List(List(0),List(1,2),List()),
        List(List(Some(0),Some(1),None), 
             List(Some(0),Some(2),None)))
    testZipNOpt(List(List(0),List(),List(1,2),List(),List(3)),
        List(List(Some(0),None,Some(1),None,Some(3)), 
             List(Some(0),None,Some(2),None,Some(3))))
  }
  
  describe("zipN with looooong lists") {
    val looongStream = (1 to 1000000000).toStream
    it(s"Should be able to zip a stream of loong elements and take 1 value") {
      zipNOption(Stream(Stream(1,2),Stream(),looongStream)).take(1) should be(Stream(Stream(Some(1),None,Some(1))))
    }
    it(s"Should be able to zip a stream of loong elements and take 2 values") {
      zipNOption(Stream(Stream(1,2),Stream(),looongStream)).take(2) should be(
          Stream(Stream(Some(1),None,Some(1)),
                 Stream(Some(1),None,Some(2))))
    }
  }
 
  def testZipN[A](vs: List[List[A]], expected: List[List[A]]) = {
   it(s"zipN($vs) should be $expected") {
    zipN(vs) should be(expected) 
   }
  }

  def testZipNOpt[A](vs: List[List[A]], expected: List[List[A]]) = {
   it(s"zipNOption($vs) should be $expected") {
    zipNOption(vs) should be(expected) 
   }
  }
  describe("mergeSeqs") {
    def fn(x:Int,y:Int): String = s"$x-$y"
    def z(x: Int): String = s"$x"
    testMergeSeqs(Seq(1,2,3),Seq(8,10),fn,z,Seq("1-8","1-10","2-8","2-10","3-8","3-10"))
    testMergeSeqs(Seq(),Seq(8,10),fn,z,Seq("8","10"))
    testMergeSeqs(Seq(1,2),Seq(),fn,z,Seq("1","2"))
  }
  
  def testMergeSeqs[A,B](s1: Seq[A], s2: Seq[A], comb: (A,A) => B, z: A => B, expected: Seq[B]) = {
   it(s"mergeSeqs($s1,$s2) should be $expected") {
    mergeSeqs(s1,s2,comb,z) should be(expected) 
   }
  }

}