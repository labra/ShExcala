import sbt._
import sbt.Keys._
import AssemblyKeys._
import ScoverageSbtPlugin._

lazy val CompatTest = config("compat") extend (Test)

lazy val root = (project in file(".")).
  configs(CompatTest).
  settings(inConfig(CompatTest)(Defaults.testSettings): _*).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, shexTestsFolder, shaclTestsFolder),
    buildInfoPackage := "buildinfo"
)

name := "shexcala"

organization := "es.weso"

version := "0.6.8"

scalaVersion := "2.11.7"

lazy val wesinVersion = "0.4.7"

lazy val shexTestsFolder = settingKey[String]("Folder where shex tests are downloaded") 

lazy val shaclTestsFolder = settingKey[String]("Folder where shacl tests are downloaded")

shexTestsFolder := "shexTests"

shaclTestsFolder := "shaclTests" 


libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "jline" % "jline" % "2.12.1"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value  
  , "io.argonaut" %% "argonaut" % "6.1"
  , "com.casualmiracles" %% "treelog" % "1.2.4"
  , "org.scalactic" % "scalactic_2.11" % "2.2.4"
  , "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  , "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
  , "org.scalatest" % "scalatest_2.11" % "2.2.4" % CompatTest
  
//  , "com.github.axel22" %% "scalameter" % "0.5-M2" % "test"
//  , "org.typelevel" %% "scalaz-scalatest" % "0.2.2" % "test"
  , "es.weso" % "wesin_2.11" % wesinVersion excludeAll(ExclusionRule(organization = "org.slf4j"))

  , "org.slf4j" % "slf4j-simple" % "1.6.4"
)

autoCompilerPlugins := true

// For sbt-native-packager

enablePlugins(SbtNativePackager)
enablePlugins(JavaAppPackaging)
enablePlugins(WindowsPlugin)

// general package information (can be scoped to Windows)
maintainer := "Jose Emilio Labra Gayo <labra@uniovi.es>"
packageSummary := "shexcala"
packageDescription := """Shape Expressions Library in Scala"""

// wix build information
wixProductId := "39b564d5-d381-4282-ada9-87244c76e14b"
wixProductUpgradeId := "6a710435-9af4-4adb-a597-98d3dd0bade1"
// The same numbers as in the docs?
// wixProductId := "ce07be71-510d-414a-92d4-dff47631848a"
// wixProductUpgradeId := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"

// For performance test...

lazy val PerfTest = config("perf").extend(Test)

testFrameworks in PerfTest += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered in PerfTest := false

parallelExecution in PerfTest := false

net.virtualvoid.sbt.graph.Plugin.graphSettings

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
//  val shaclTests = url("https://github.com/w3c/data-shapes/raw/gh-pages/data-shapes-test-suite/tests.zip")
//  IO.unzipURL(shaclTests, resourceManaged.value / shaclTestsFolder.value).toSeq
//}.taskValue

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// Eclipse

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

// Publishing settings to BinTray

publishMavenStyle := true

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// Scalariform (default disables formatting)
// defaultScalariformSettings

// to enable formatting, use: 
// scalariformSettings
