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
 
 describe("W3c tests report") {
   for ((r,n) <- report.items zip (1 to report.items.length))
   it("Should pass test " + n + ": " + r.name) {
     if (r.passed) 
       info("Info: " + r)
     else 
       fail("Test did not pass" + r)
   } 
 }
 
 describe("Generate W3c EARL report") {
    val passedCount = 3 // Number of tests that have to be passed
    it("Should Generate EARL report with " + passedCount + " passed values") {
      val earlModel = report.generateEARL
      val conf : Config = ConfigFactory.load()
      val outFile = conf.getString("EarlReportFile")

      earlModel.write(new FileOutputStream(outFile),"TURTLE")

      val readModel = ModelFactory.createDefaultModel()
      readModel.read(new FileInputStream(outFile),"","TURTLE")
      val earl		= "http://www.w3.org/ns/earl#"
      val earl_outcome = readModel.createProperty(earl + "outcome")
      val earl_passed  = readModel.createResource(earl + "passed")
      val passed = readModel.listResourcesWithProperty(earl_outcome, earl_passed).toList.asScala
      passed.length should be(passedCount)
   }
 }

}