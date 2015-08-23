import sbt._
import sbt.Keys._
import AssemblyKeys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._
import ScoverageSbtPlugin._

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, shexTestsFolder, shaclTestsFolder),
    buildInfoPackage := "buildinfo"
  )

name := "shExcala"

organization := "es.weso"

version := "0.4.0"

scalaVersion := "2.11.7"

lazy val shexTestsFolder = settingKey[String]("Folder where shex tests are downloaded") 

lazy val shaclTestsFolder = settingKey[String]("Folder where shacl tests are downloaded")

shexTestsFolder := "shexTests"

shaclTestsFolder := "shaclTests" 

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "jline" % "jline" % "2.12.1"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value  
  , "org.apache.jena" % "jena-arq" % "2.13.0" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "org.scalaz" % "scalaz-core_2.11" % "7.1.2" 
  , "org.spire-math" %% "cats" % "0.1.2"
  , "io.argonaut" %% "argonaut" % "6.1"
  , "com.casualmiracles" %% "treelog" % "1.2.4"
  , "org.scalactic" % "scalactic_2.11" % "2.2.4"
  , "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  , "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
  , "com.github.axel22" %% "scalameter" % "0.5-M2" % "test"
  , "org.typelevel" %% "scalaz-scalatest" % "0.2.2" % "test"
  , "es.weso" % "wesin_2.11" % "0.3.7" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "org.slf4j" % "slf4j-simple" % "1.6.4"
//  , "org.w3" % "banana-rdf_2.11" % "0.8.1"
)

autoCompilerPlugins := true

// For performance test...

lazy val PerfTest = config("perf").extend(Test)

testFrameworks in PerfTest += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered in PerfTest := false

parallelExecution in PerfTest := false

seq(bintraySettings:_*)

deploymentSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

// publish <<= publish.dependsOn(publish in config("universal"))

packageArchetype.java_application

// Publish site info
site.settings

site.includeScaladoc()

lazy val scoverageSettings = Seq(
  ScoverageKeys.coverageMinimum := 50,
  ScoverageKeys.coverageFailOnMinimum := false
)

// Testing 

//resourceGenerators in Test += Def.task {
//  val shexTests = url("https://github.com/shexSpec/test-suite/raw/gh-pages/tests.zip")
//  IO.unzipURL(shexTests, resourceManaged.value / shexTestsFolder.value).toSeq
  // val shaclTests = url("https://github.com/w3c/data-shapes/raw/gh-pages/data-shapes-test-suite/tests.zip")
  // IO.unzipURL(shaclTests, resourceManaged.value / shaclTestsFolder.value).toSeq
//}.taskValue

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// Eclipse

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

// Publishing settings to BinTray

bintraySettings

publishMavenStyle := true

repository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// Scalariform (default disables formatting)

defaultScalariformSettings

// to enable formatting, use: 
// scalariformSettings
