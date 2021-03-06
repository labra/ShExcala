package es.weso.shex.validation

import es.weso.shex.ManifestRunner
import org.scalatest._
import es.weso.manifest._
import com.typesafe.config._
import util._
import es.weso.utils.FileUtils._


class OneFromManifest extends RDF2Manifest
    with FunSpecLike
    with Matchers
    with TryValues 
    with ManifestRunner {
  
  val name = "iovka"
  
  val conf: Config = ConfigFactory.load()
  
  val validationFolder = conf.getString("validationFolder")

  val manifestFile = validationFolder + "manifest.ttl"
  val base = filePath2URI(validationFolder)
  
  describe("Running tests folder") {
    describe(s"Can read and execute tests in $manifestFile with base $base") {
    val maybeManifest = RDF2Manifest.read(manifestFile,base) 
    maybeManifest match {
      case Success(manifest) => {
        runTestByName(manifest,base,name)
      }
      case Failure(e) => Assertions.fail("Exception reading manifest file (" + manifestFile + "): " + e.getMessage)
     }
    }
  }


}
