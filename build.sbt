import sbt._
import sbt.Keys._
import AssemblyKeys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

lazy val root = project.in(file(".")).configs( PerfTest ).settings(inConfig(PerfTest)(Defaults.testTasks): _*)

name := "shExcala"

organization := "es.weso"

version := "0.2.1"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value  
  , "org.apache.jena" % "jena-arq" % "2.10.1" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "org.scalaz" % "scalaz-core_2.11" % "7.1.0" 
  , "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
  ,	"org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
  , "com.github.axel22" %% "scalameter" % "0.5-M2" % "test"
  , "es.weso" % "wesin_2.11" % "0.2.1" excludeAll(ExclusionRule(organization = "org.slf4j"))
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

publishMavenStyle := true

net.virtualvoid.sbt.graph.Plugin.graphSettings

// publish <<= publish.dependsOn(publish in config("universal"))

packageArchetype.java_application

resourceGenerators in Test += Def.task {
  val location = url("https://github.com/shexSpec/test-suite/raw/gh-pages/tests.zip")
  IO.unzipURL(location, resourceManaged.value / "downloadedTests").toSeq
}.taskValue

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// for BuildInfo

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "buildInfo"

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

bintraySettings

scalariformSettings

repository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"
