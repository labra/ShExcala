import sbt._
import sbt.Keys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

lazy val root = project.in(file("."))//.settings(crossScalaVersions := Seq("2.10.4", "2.11.0"))

Build.sharedSettings

version := Build.currentVersion

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.3.7" % "test" 
  , "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "org.scala-lang" % "scala-compiler" % "2.10.2" 
  , "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.2"
  , "org.apache.jena" % "jena-arq" % "2.10.1" 
  , "org.scalaz" % "scalaz-core_2.10" % "7.0.6" 
  , "es.weso" % "stateparser_2.10" % "0.0.1"
  , "es.weso" % "wesin_2.10" % "0.0.1" 
  )

autoCompilerPlugins := true

bintraySettings

Build.publishSettings

resourceGenerators in Test += Def.task {
  val location = url("https://github.com/shexSpec/test-suite/raw/gh-pages/tests.zip")
  IO.unzipURL(location, resourceManaged.value / "downloadedTests").toSeq
}.taskValue

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)





