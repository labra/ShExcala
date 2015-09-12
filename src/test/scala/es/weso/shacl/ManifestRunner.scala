package es.weso.shacl

import es.weso.manifest.{Entry => ManifestEntry, _}
import org.scalatest._
import es.weso.utils.Logging
import es.weso.rdf.RDFReader
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Shacl._
import util._
import es.weso.utils.IO._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.monads.Result._


trait ManifestRunner 
 extends FunSpecLike
     with Matchers
     with Logging {
  
  def runTests(mf: Manifest, base: String): Unit = {
    for(entry <- mf.entries) {
      it ("Entry: " + entry.name) {
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
          matcher <- Try{ ShaclMatcher(schema,rdf) }
          result <- Try(matcher.match_node_label(node)(label))
          _ <- Try(info("Result " + result))
          expected <- Try(entry.result.asBoolean.get)
        } yield (result,expected)

        attempt match {
          case Success((result,expected)) => {
            val validationResult : Boolean = result.isValid
            validationResult should be(expected) 
          } 
          case Failure(e) => fail("Exception: " + e.getMessage)
        }
      }
      case _ => {
        log.error("Unsupported entry type: " + entry.entryType)
      }
    }
  }
  
  def extractSchema(action: ManifestAction, base: String): Try[Schema] = {
    println("Extracting schema...")
    for {
      schema <- Try(action.schema.get)
      schemaFormat <- Try(action.schemaFormat.getOrElse(SchemaFormats.default))
      val schemaFilename = relativize(schema, base)
      (schema,pm) <- Schema.fromFile(schemaFilename, schemaFormat)
    } yield {
      println("Schema: " + schema)
      schema
    }
  }

  def extractRDF(action: ManifestAction, base: String): Try[RDFReader] = {
    println("Extracting data...")
    for {
      data <- Try(action.data.get)
      dataFormat <- Try(action.dataFormat.getOrElse(DataFormats.default))
      val dataFilename = relativize(data, base)
      cs <- getContents(dataFilename)
      rdf <- RDFAsJenaModel.fromChars(cs,dataFormat)
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
    val iriLocal = iri.str.stripPrefix("http://www.w3.org/ns/")
    base + iriLocal
  }
  
}
