package es.weso.performance


import es.weso.monads.Result
import org.scalameter.api._

object TestPSets extends PerformanceTest.Quickbenchmark {
   
  val sizes: Gen[Int] = Gen.range("size")(1, 10, 2)
  
  val ranges: Gen[Range] = for {
  size <- sizes
  } yield 0 until size
  
  performance of "Range" in {
  measure method "map" in {
    using(ranges) in {
      r => r.map(_ + 1)
    }
  }
}
}