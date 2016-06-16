package es.weso.shex

import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.rdf.model.Model
import org.apache.jena.datatypes.xsd.XSDDatatype
import java.util.Calendar
import java.text.SimpleDateFormat

case class Report(items: List[SingleTestReport]) {
  def addTestReport(r: SingleTestReport): Report = {
    Report(r :: items)
  }

  def addTestReport(passed: Boolean, name: String, uriTest: String, testType: String, msg: String): Report = {
    Report(SingleTestReport(passed, name, uriTest, testType, msg) :: items)
  }

  def concat(other: Report): Report = {
    Report(items ++ other.items)
  }

  def generateEARL: Model = {
    val model = ModelFactory.createDefaultModel

    val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

    val foaf = "http://xmlns.com/foaf/0.1/"
    val doap = "http://usefulinc.com/ns/doap#"
    val wesinURL = "http://purl.org/weso/wesin"
    val turtleURL = "http://www.w3.org/TR/turtle/"
    val rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    val earl = "http://www.w3.org/ns/earl#"
    val dc = "http://purl.org/dc/terms/"
    val rdfs = "http://www.w3.org/2000/01/rdf-schema#"
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val turtleTests = "http://www.w3.org/2013/TurtleTests/manifest.ttl#"

    model.setNsPrefix("doap", doap)
    model.setNsPrefix("rdf", rdf)
    model.setNsPrefix("rdfs", rdfs)
    model.setNsPrefix("foaf", foaf)
    model.setNsPrefix("earl", earl)
    model.setNsPrefix("dc", dc)
    model.setNsPrefix("ttlTests", turtleTests)

    val rdf_type = model.createProperty(rdf + "type")

    val foaf_name = model.createProperty(foaf + "name")
    val foaf_homepage = model.createProperty(foaf + "homepage")
    val foaf_primaryTopic = model.createProperty(foaf + "primaryTopic")
    val foaf_maker = model.createProperty(foaf + "maker")

    val doapProject = model.createResource(doap + "Project")
    val doapVersion = model.createResource(doap + "Version")

    val doap_name = model.createProperty(doap + "name")
    val doap_license = model.createProperty(doap + "license")
    val doap_developer = model.createProperty(doap + "developer")
    val doap_maintainer = model.createProperty(doap + "maintainer")
    val doap_documenter = model.createProperty(doap + "documenter")
    val doap_maker = model.createProperty(doap + "maker")
    val doap_homePage = model.createProperty(doap + "homepage")
    val doap_implements = model.createProperty(doap + "implements")
    val doap_downloadPage = model.createProperty(doap + "download-page")
    val doap_programmingLanguage = model.createProperty(doap + "programming-language")
    val doap_release = model.createProperty(doap + "release")
    val doap_created = model.createProperty(doap + "created")

    val dc_issued = model.createProperty(dc + "issued")
    val dc_title = model.createProperty(dc + "title")
    val dc_description = model.createProperty(dc + "description")
    val dc_date = model.createProperty(dc + "date")
    val dc_creator = model.createProperty(dc + "creator")

    val earlSoftware = model.createResource(earl + "Software")
    val earlTestSubject = model.createResource(earl + "TestSubject")
    val earlAssertion = model.createResource(earl + "Assertion")
    val earlTestResult = model.createResource(earl + "TestResult")

    val earl_automatic = model.createResource(earl + "automatic")

    val earl_assertedBy = model.createProperty(earl + "assertedBy")
    val earl_subject = model.createProperty(earl + "subject")
    val earl_test = model.createProperty(earl + "test")
    val earl_result = model.createProperty(earl + "result")
    val earl_mode = model.createProperty(earl + "mode")
    val earl_outcome = model.createProperty(earl + "outcome")
    val earl_passed = model.createProperty(earl + "passed")
    val earl_failed = model.createProperty(earl + "failed")

    val labra = model.createResource("http://www.di.uniovi.es/~labra#me")
    val release = model.createResource()
    val wesin = model.createResource(wesinURL)
    val thisReport = model.createResource("")

    val now = model.createTypedLiteral(sdf.format(Calendar.getInstance.getTime), XSDDatatype.XSDdateTime)

    model.add(labra, foaf_name, "Jose Emilio Labra Gayo")
    model.add(labra, foaf_homepage, "http://www.di.uniovi.es/~labra")

    model.add(thisReport, foaf_maker, labra)
    model.add(thisReport, foaf_primaryTopic, wesin)

    model.add(thisReport, dc_issued, now)

    model.add(wesin, rdf_type, doapProject)
    model.add(wesin, rdf_type, earlSoftware)
    model.add(wesin, rdf_type, earlTestSubject)
    model.add(wesin, doap_name, "Wesin")
    model.add(wesin, doap_homePage, wesinURL)
    model.add(wesin, doap_implements, turtleURL)
    model.add(wesin, doap_developer, labra)
    model.add(wesin, doap_maintainer, labra)
    model.add(wesin, doap_documenter, labra)
    model.add(wesin, doap_maker, labra)
    model.add(wesin, doap_downloadPage, wesinURL)
    model.add(wesin, doap_programmingLanguage, "Scala")
    model.add(wesin, dc_title, "Wesin")
    model.add(wesin, dc_date, model.createTypedLiteral("2013-08-29", XSDDatatype.XSDdateTime))
    model.add(wesin, dc_creator, labra)
    model.add(wesin, dc_description, model.createLiteral("WESIN - Web Semantics using Inductive Graphs", "en"))
    model.add(wesin, doap_release, release)

    model.add(release, doap_name, "Wesin")
    model.add(release, doap_created,
      model.createTypedLiteral("2013-08-28", XSDDatatype.XSDdate))
    model.add(release, rdf_type, doapVersion)

    // Information about a test item 
    for (r <- items) {
      val t = model.createResource()
      val result = model.createResource()
      model.add(t, rdf_type, earlAssertion)
      model.add(t, earl_test, model.createResource(turtleTests + r.uriTest))
      model.add(t, foaf_name, r.name)
      model.add(t, earl_assertedBy, labra)
      model.add(t, earl_mode, earl_automatic)
      model.add(t, earl_subject, wesin)
      model.add(t, earl_result, result)
      model.add(result, rdf_type, earlTestResult)
      val outcome = if (r.passed) earl_passed
      else earl_failed
      model.add(result, earl_outcome, outcome)
      model.add(result, dc_date, now)
    }

    model
  }
}

object Report {
  def initial = Report(List())

  def sortReport(r1: SingleTestReport, r2: SingleTestReport): Boolean = {
    r1.name < r2.name
  }
}

case class SingleTestReport(
    passed: Boolean, // True if test passed
    name: String, // Name of test
    uriTest: String, // URI of test
    testType: String, // Type of test
    moreInfo: String // Info about what happened
    ) {

  override def toString: String =
    if (passed) testType + ". OK " + name +
      ", uri: " + uriTest + ". " + moreInfo
    else testType + ". Failed " + name +
      ", uri: " + uriTest + ". " + moreInfo
}

