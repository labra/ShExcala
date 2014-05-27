package es.weso.shex

import org.scalatest.FunSpec
import com.typesafe.config._
import com.hp.hpl.jena.rdf.model.ModelFactory
import java.io.FileOutputStream
import java.io.FileInputStream
import scala.collection.JavaConverters._
import org.scalatest.Matchers


class RunTestsFolderSpec extends FunSpec with Matchers {
 val report = RunTestsFolder.createReport
 
 
 describe("test-suite report") {
   info("Running tests from " + RunTestsFolder.testsDir)
   info("Manifest file: " + RunTestsFolder.manifestFile)

   for ((r,n) <- report.items.sortWith(Report.sortReport) zip (1 to report.items.length))
   it(r.name + ". " + n ) {
     if (!r.passed) 
       fail("Test failed: " + r)
   } 
 }
 
 describe("Generate W3c EARL report") {

   it("Should Generate EARL report ") {
      val earlModel = report.generateEARL
      val conf : Config = ConfigFactory.load()
      val outFile = conf.getString("EarlReportFile")

      earlModel.write(new FileOutputStream(outFile),"TURTLE")

      val readModel = ModelFactory.createDefaultModel()
      readModel.read(new FileInputStream(outFile),"","TURTLE")
      val earl		= "http://www.w3.org/ns/earl#"
      val earl_outcome = readModel.createProperty(earl + "outcome")
      val earl_passed  = readModel.createResource(earl + "passed")
      val earl_failed  = readModel.createResource(earl + "failed")
      val passed = readModel.listResourcesWithProperty(earl_outcome, earl_passed).toList.asScala
      val failed = readModel.listResourcesWithProperty(earl_outcome, earl_failed).toList.asScala
      info("Generated report " + passed.length + " passed. " + failed.length + " failed. File: " + outFile)
   }
 }

}