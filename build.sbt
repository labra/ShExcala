import sbt._
import sbt.Keys._
import AssemblyKeys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

lazy val root = project.in(file("."))//.settings(crossScalaVersions := Seq("2.10.4", "2.11.0"))

Build.sharedSettings

version := Build.currentVersion

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.3.7" % "test" 
  , "org.slf4j" % "slf4j-simple" % "1.6.4"
  , "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "org.scala-lang" % "scala-compiler" % "2.10.2" 
  , "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.2"
  , "org.apache.jena" % "jena-arq" % "2.10.1" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "org.scalaz" % "scalaz-core_2.10" % "7.0.6" 
  , "es.weso" % "wesin_2.10" % "0.1.4" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "com.github.axel22" %% "scalameter" % "0.5-M2"
  )

autoCompilerPlugins := true

seq(bintraySettings:_*)

net.virtualvoid.sbt.graph.Plugin.graphSettings

Build.publishSettings

deploymentSettings

publishMavenStyle := true

// publish <<= publish.dependsOn(publish in config("universal"))

packageArchetype.java_application

//resourceGenerators in Test += Def.task {
//  val location = url("https://github.com/shexSpec/test-suite/raw/gh-pages/tests.zip")
//  IO.unzipURL(location, resourceManaged.value / "downloadedTests").toSeq
//}.taskValue

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

testFrameworks += new TestFramework(
    "org.scalameter.ScalaMeterFramework")

logBuffered := false

// Scalameter benchmark needs to run tests sequentially 
parallelExecution in Test := false
//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

