package es.weso.shex

import org.apache.jena.rdf.model.{
ModelFactory,
Model,
Resource,
Property,
RDFNode => JenaNode
}
import es.weso.rdf.nodes._
import es.weso.monads.Result._
import com.typesafe.config._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.io.Source._
import es.weso.rdf.jena.JenaMapper
import java.io.IOException
import java.io.FileNotFoundException
import scala.util._
import java.net.URI
import scala.collection.JavaConverters._
import buildinfo._
import es.weso.manifest._
import org.scalatest.FunSpecLike
import es.weso.utils.FileUtils._

class RunTestsFolder
  extends ManifestRunner
    with FunSpecLike {

  val conf: Config = ConfigFactory.load()
  val testsDir = conf.getString("shaclTestsFolder")
  val manifestFile = testsDir + "manifest.ttl"
  val base = filePath2URI(testsDir)


  describe("Running tests folder") {
    describe("Can read and execute tests in " + manifestFile) {
      val maybeManifest = RDF2Manifest.read(manifestFile, base)
      maybeManifest match {
        case Success(manifest) => runTests(manifest, base)
        case Failure(e) => fail("Exception reading manifest file (" + manifestFile + "): " + e.getMessage)
      }
    }
  }


}

