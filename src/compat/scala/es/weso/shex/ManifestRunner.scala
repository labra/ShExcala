package es.weso.shex

import es.weso.manifest.{Entry => ManifestEntry, _}
import org.scalatest._
import es.weso.utils.Logging
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import es.weso.shex.ShEx._

import util._
import es.weso.utils.IO._
import es.weso.rdf.jena.RDFAsJenaModel

trait ManifestRunner 
 extends FunSpecLike
     with Matchers
     with Logging {
  
  def runTestByName(mf: Manifest, base: String, name:String): Unit = {
    it ("Should pass entry: " + name) {
    val entriesWithName = mf.entries.filter(_.name == name)
    if (entriesWithName.isEmpty) {
      fail(s"Not found entry $name")  
    } else 
        runEntry(entriesWithName.head, base)      
    }
  }

  def runTests(mf: Manifest, base: String): Unit = {
    for(entry <- mf.entries) {
      it ("Should pass entry: " + entry.name) {
        runEntry(entry, base)      
      }
    }
  }
  
  def runEntry(entry: ManifestEntry, base: String): Unit = {
    entry.entryType match {
      case MatchNodeShape => {
        info("Entry: " + entry.name + " - result " + entry.result)
        val action = entry.action
        info("Action: " + action)
        val attempt = for {
          schema <- extractSchema(action, base)
          rdf <- extractRDF(action, base)
          node <- extractNode(action)
          label <- extractLabel(action)
          matcher <- Try{ ShExMatcher(schema,rdf) }
          result <- Try(matcher.match_node_label(node)(label))
          _ <- Try(info("Result " + result))
          expected <- Try(entry.result.asBoolean.get)
        } yield (result,expected)

        attempt match {
          case Success((result,expected)) => {
            val r : ShExResult = result
            val validationResult : Boolean = result.isValid
            validationResult should be(expected) 
          } 
          case Failure(e) => fail("Exception: " + e.getMessage)
        }
      }
      case ValidationTest => {
        info("Entry: " + entry.name + " - result " + entry.result)
        val action = entry.action
        info("Action: " + action)
        val attempt = for {
          schema <- extractSchema(action, base)
          rdf <- extractRDF(action, base)
          node <- extractNode(action)
          label <- extractLabel(action)
          matcher <- Try{ ShExMatcher(schema,rdf) }
          result <- Try(matcher.match_node_label(node)(label))
          _ <- Try(info("Result " + result))
          expected <- Try(entry.result.asBoolean.getOrElse(true))
        } yield (result,expected)

        attempt match {
          case Success((result,expected)) => {
            val validationResult : Boolean = result.isValid
            validationResult should be(expected) 
          } 
          case Failure(e) => fail("Exception: " + e.getMessage)
        }
      }
      case ValidationFailure => {
        info("Entry to fail: " + entry.name + " must fail. Result " + entry.result)
        val action = entry.action
        info("Action to fail: " + action)
        val attempt = for {
          schema <- extractSchema(action, base)
          rdf <- extractRDF(action, base)
          node <- extractNode(action)
          label <- extractLabel(action)
          matcher <- Try{ ShExMatcher(schema,rdf) }
          result <- Try(matcher.match_node_label(node)(label))
          _ <- Try(info("Result that should be a failure: " + result))
        } yield (result)

        attempt match {
          case Success(result) => {
            val validationResult : Boolean = result.isValid
            info("Checking that it fails")
            validationResult should be(false) 
          } 
          case Failure(e) => info("Exception: " + e.getMessage)
        }
      }
      case _ => {
        log.error("Unsupported entry type: " + entry.entryType)
      }
    }
  }
  
  def extractSchema(action: ManifestAction, base: String): Try[Schema] = {
    println(s"Extracting schema...${action.schema.get.str} with base $base")
    for {
      schema <- Try(action.schema.get)
      schemaFormat <- Try(action.schemaFormat.getOrElse(SchemaFormat.default.name))
      val schemaFilename = relativize(schema, base)
      (schema,pm) <- {
         println(s"Schema file name: $schemaFilename")
         Schema.fromURI(new java.net.URI(schemaFilename), schemaFormat, Some(base))
        }
    } yield {
      println("Schema: " + schema)
      schema
    }
  }

  def extractRDF(action: ManifestAction, base: String): Try[RDFReader] = {
    println(s"Extracting RDF...${action.data.get.str} with base $base")
    for {
      data <- Try(action.data.get)
      dataFormat <- Try(action.dataFormat.getOrElse(DataFormat.default.name))
      val dataUri = relativize(data, base)
      rdf <- RDFAsJenaModel.fromURI(dataUri,dataFormat,Some(base))
    } yield {
      println("RDF: " + rdf)
     rdf 
    }
  }
  
  def extractNode(action: ManifestAction): Try[RDFNode] = {
    action.node match {
      case Some(n) => Success(n)
      case None => Failure(throw new Exception("Cannot extract node from action " + action))
    }
  }
  
  def extractLabel(action:ManifestAction): Try[Label] = {
    action.shape match {
      case Some(str) => Success(IRILabel(str))
      case None => Failure(throw new Exception("Cannot extract label from action " + action))
    }
  }
  
  def relativize(iri: IRI, base: String): String = {
//    val iriLocal = iri.str.stripPrefix("http://www.w3.org/ns/")
//    base + iriLocal
    iri.str
  }
  
}
