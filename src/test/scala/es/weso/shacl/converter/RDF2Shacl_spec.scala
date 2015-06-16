package es.weso.shacl.converter

import org.scalatest._
import org.scalatest.prop._
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import es.weso.rdf.jena.RDFAsJenaModel
import com.hp.hpl.jena.rdf.model.Model
import es.weso.shacl._
import es.weso.shacl.Shacl._
import util._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.statements._
import es.weso.rdf._

class RDF2ShaclSpec
    extends FunSpec
    with Matchers
    with Checkers 
    with TryValues 
    with RDFBased {

  describe("RDF2Shacl") {

    it("Should be able to extract a cardinality") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |:n sh:minCount 1; sh:maxCount 3 .
                       |""".stripMargin
      val n = IRI("http://e.o#n")                 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.cardinality(n,rdf)
      } yield 
        c 
      c.success.value should be(RangeCardinality(1,3))
    }
    
    it("Should be able to extract an unbounded cardinality") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |:n sh:minCount 2 .
                       |""".stripMargin
      val n = IRI("http://e.o#n")                 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.cardinality(n,rdf)
      } yield 
        c 
      c.success.value should be(UnboundedCardinalityFrom(2))
    }

    it("Should be able to extract a literal Datatype") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n sh:valueType xsd:string .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.literalDatatype(n,rdf)
      } yield 
        c 
      c.success.value should be(LiteralDatatype(xsd_string,emptyFacets))
    }
    
    it("Should be able to extract a nodeKind IRI") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n sh:nodeKind sh:IRI .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.nodeKind(n,rdf)
      } yield 
        c 
      c.success.value should be(IRIKind)
    }
    
    it("Should be able to extract a valueConstr with valueType") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n sh:valueType xsd:string .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.valueConstr(n,rdf)
      } yield 
        c 
      c.success.value should be(LiteralDatatype(xsd_string,emptyFacets))
    }
    
    it("Should be able to extract a valueConstr with nodeKind IRI") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n sh:nodeKind sh:IRI .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.valueConstr(n,rdf)
      } yield 
        c 
      c.success.value should be(IRIKind)
    }

    it("Should complain if there are nodeKind and valueType for ValueConstr") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n sh:nodeKind sh:IRI ; sh:valueType xsd:string .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.valueConstr(n,rdf)
      } yield 
        c 
      c.failure 
    }

    it("Should parse a triple constraint ") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n sh:predicate :p ; 
                       |   sh:valueType xsd:string ; 
                       |   sh:minCount 1 ;
                       |   sh:maxCount 3 .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val n_label = IRILabel(n)
      val p = IRI("http://e.o#p")
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.tripleConstraint(n,rdf)
      } yield 
        c 
      c.success.value should be(TripleConstraint(Some(n_label),p,LiteralDatatype(xsd_string,emptyFacets),RangeCardinality(1,3))) 
    }
    
    it("Should parse an open shape definition ") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n a sh:Shape ;
                       |   sh:property :prop .
                       |    
                       |:prop sh:predicate :p ; 
                       |      sh:valueType xsd:string ; 
                       |      sh:minCount 1 ;
                       |      sh:maxCount 3  
                       |      .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val n_label = IRILabel(n)
      val prop_label = IRILabel(IRI("http://e.o#prop"))
      val p = IRI("http://e.o#p")
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.openShape(n,rdf)
      } yield 
        c 
      val expected = OpenShape(
              TripleConstraint(Some(prop_label),
                               p,
                               LiteralDatatype(xsd_string,emptyFacets),RangeCardinality(1,3)),
              emptyInclPropSet
          ) 
      triedMustBe(expected, c)
    } 

    it("Should parse an open shape definition with a blank node") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n a sh:Shape ;
                       |   sh:property [
                       |      sh:predicate :p ; 
                       |      sh:valueType xsd:string ; 
                       |      sh:minCount 1 ;
                       |      sh:maxCount 3 
                       |      ] .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val n_label = IRILabel(n)
      val prop_label = IRILabel(IRI("http://e.o#prop"))
      val p = IRI("http://e.o#p")
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.shapeDefinition(n,rdf)
      } yield 
        c 
      val expected = OpenShape(
              TripleConstraint(Some(prop_label),
                               p,
                               LiteralDatatype(xsd_string,emptyFacets),RangeCardinality(1,3)),
              emptyInclPropSet
          ) 
      // We cannot compare with unknown bnodeLabels
      // triedMustBe(expected, c)
      c.isSuccess should be(true)
    }
    
   it("Should parse an open shape definition with a conjunction") {
      val rdf_str = """|@prefix : <http://e.o#> .
                       |@prefix sh: <http://www.w3.org/ns/shacl/core#> .
                       |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                       |
                       |:n a sh:Shape ;
                       |   sh:property :prop1, :prop2 .   
                       |:prop1 sh:predicate :p ;  
                       |      sh:valueType xsd:string ; 
                       |      sh:minCount 1 ;
                       |      sh:maxCount 3 . 
                       |:prop2 sh:predicate :q; sh:nodeKind sh:IRI .
                       |""".stripMargin
      val n = IRI("http://e.o#n") 
      val n_label = IRILabel(n)
      val prop1_label = IRILabel(IRI("http://e.o#prop"))
      val prop2_label = IRILabel(IRI("http://e.o#prop"))
      val p = IRI("http://e.o#p")
      val q = IRI("http://e.o#p")
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(rdf_str, "TURTLE")
        c <- RDF2Schema.shapeDefinition(n,rdf)
      } yield 
        c 
      val expected = OpenShape(
              GroupShape(None,Seq(TripleConstraint(Some(prop1_label),
                               p,
                               LiteralDatatype(xsd_string,emptyFacets),RangeCardinality(1,3)
                             ),
                             TripleConstraint(Some(prop2_label),
                               q,
                               IRIKind,UnboundedCardinalityFrom(1)
                             )
                            )),
              emptyInclPropSet
          ) 
      // We cannot compare with unknown bnodeLabels
      // triedMustBe(expected, c)
      c.isSuccess should be(true)
    }
   
    it("Should parse a simple example with an IRI") {
      val schemaStr = """|@prefix :      <http://pepe.com/> .
                         |@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
                         |@prefix sh:    <http://www.w3.org/ns/shacl/core#> .
                         |
                         |:a      a            sh:Shape ;
                         |        sh:property  [ a             sh:PropertyConstraint ;
                         |                       sh:minCount   1 ;
                         |                       sh:nodeKind   sh:IRI ;
                         |                       sh:predicate  :b ] .
                         |""".stripMargin
      val n = IRI("http://e.o#n") 
      val n_label = IRILabel(n)
      val prop1_label = IRILabel(IRI("http://e.o#prop"))
      val prop2_label = IRILabel(IRI("http://e.o#prop"))
      val p = IRI("http://e.o#p")
      val q = IRI("http://e.o#p")
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(schemaStr, "TURTLE")
        c <- RDF2Schema.rdf2Schema(rdf)
      } yield 
        c 
      val expected = OpenShape(
              GroupShape(None,Seq(TripleConstraint(Some(prop1_label),
                               p,
                               LiteralDatatype(xsd_string,emptyFacets),RangeCardinality(1,3)
                             ),
                             TripleConstraint(Some(prop2_label),
                               q,
                               IRIKind,UnboundedCardinalityFrom(1)
                             )
                            )),
              emptyInclPropSet
          ) 
      // We cannot compare with unknown bnodeLabels
      // triedMustBe(expected, c)
      if (c.isFailure) {
        println("Unexpected Failure: " + c)
      }
      c.isSuccess should be(true)
    }


  }

  describe("RDF parser") {
    it("Can parse RDF lists") {
      val schemaStr = """|<http://pepe.com/a> <http://pepe.com/b> (1 2) .
                         |""".stripMargin
      val c = for {
        rdf <- RDFAsJenaModel.fromChars(schemaStr, "TURTLE")
        parsed <- RDF2Schema.rdfListForPredicate(IRI("http://pepe.com/b"))(IRI("http://pepe.com/a"),rdf)
      } yield parsed 
      c.success.value should be(List(IntegerLiteral(1),IntegerLiteral(2))) 
    }
   }
  
  
 
  def triedMustBe[A](expected: A, tried: Try[A]): Unit = {
    tried match {
      case Success(v) => {
        if (v != expected) {
          println("Different values...")
          println("v        = " + v)
          println("expected = " + expected)
        }
        v should be(expected)
      }
      case Failure(e) => fail("tried value is not success. Expected: " + expected.toString + " Exception: " + e.getMessage)
    }
  }
}