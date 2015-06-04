package es.weso.shex

import org.scalatest.FunSpec
import com.typesafe.config._
import com.hp.hpl.jena.rdf.model.ModelFactory
import java.io.FileOutputStream
import java.io.FileInputStream
import scala.collection.JavaConverters._
import org.scalatest.Matchers
import es.weso.shacl.Report

class RunTestsBack extends FunSpec with Matchers {
  val runner = RunTestsFolder(ShapeValidatorBacktracking)
  val report = runner.createReport

  describe("test-suite report for ShapeValidator with backtracking") {
    info("Running tests from " + runner.testsDir)
    info("Manifest file: " + runner.manifestFile)

    for ((r, n) <- report.items.sortWith(Report.sortReport) zip (1 to report.items.length))
      it(r.name + ". " + n) {
        if (!r.passed)
          fail("Test failed: " + r)
      }
  }

}