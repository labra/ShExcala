package es.weso.performance


import es.weso.rdfgraph.nodes._
import es.weso.shex.Schema
import es.weso.shex.Typing
import es.weso.rdf.RDFTriples
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.shex.PrefixMaps
import org.scalameter.api._

object TestAnds extends PerformanceTest.Quickbenchmark {
   
  val sizes: Gen[Int] = Gen.range("size")(1, 5, 2)
  
  val pairs: Gen[(Schema,RDFTriples)] = { 
    for { size <- sizes } yield 
     (GenShape.genAnds(size), GenShape.genTriples(size)) 
  }
  
  performance of "ShapeValidator ands" in {
  measure method "matchAll" in {
    using(pairs) in {
      p => Schema.matchAll(p._2,p._1)
    }
  }
 }
}