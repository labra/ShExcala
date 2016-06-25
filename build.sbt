import sbt._
import sbt.Keys._

lazy val CompatTest = config("compat") extend (Test)

lazy val shExcala = (project in file(".")).
  configs(CompatTest).
  settings(publishSettings:_*).
  settings(inConfig(CompatTest)(Defaults.testSettings): _*).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, shexTestsFolder),
    buildInfoPackage := "buildinfo"
)

name := "shexcala"

organization := "es.weso"

version := "0.7.14"

scalaVersion := "2.11.8"

// Folders where shex tests are downloaded
lazy val shexTestsFolder = settingKey[String]("Folder where shex tests are downloaded") 

shexTestsFolder := "shexTests"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value  
//  , "com.typesafe" %% "abide-core" % "0.1-SNAPSHOT" % "abide"
  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "1.0.0"
  , "com.github.scopt" %% "scopt" % "3.5.0"
  , "com.typesafe" % "config" % "1.0.1"
  , "io.argonaut" %% "argonaut" % "6.1"
  , "org.scalatest" %%% "scalatest" % "3.0.0-M15" 
  , "org.slf4s" %% "slf4s-api" % "1.7.12"
  , "ch.qos.logback" % "logback-classic" % "1.1.2"
  
  , "es.weso" % "srdf-jena_2.11" % "0.0.6" 
  , "es.weso" % "rbe_2.11" % "0.0.4"
  , "es.weso" % "weso_utils_2.11" % "0.0.7" 
  , "es.weso" % "validating_2.11" % "0.0.12"
  , "es.weso" % "stateparser_2.11" % "0.1.3"   
)

autoCompilerPlugins := true

// For sbt-native-packager

enablePlugins(SbtNativePackager)
enablePlugins(JavaAppPackaging)
enablePlugins(WindowsPlugin)

// general package information 
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

// lazy val PerfTest = config("perf").extend(Test)

// testFrameworks in PerfTest += new TestFramework("org.scalameter.ScalaMeterFramework")

//logBuffered in PerfTest := false

// parallelExecution in PerfTest := false

// Publish site info

site.settings

site.includeScaladoc()

// coverageEnabled := true
// coverageMinimum := 50
// coverageFailOnMinimum := false

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

resolvers += Resolver.bintrayRepo("labra", "maven")


// For scoverage...
// resolvers += Resolver.url("scoverage-bintray", url("https://dl.bintray.com/sksamuel/sbt-plugins/"))(Resolver.ivyStylePatterns)

// Scalariform (default disables formatting)
// defaultScalariformSettings

// to enable formatting, use: 
// scalariformSettings

git.remoteRepo := "git@github.com:labra/ShExcala.git"

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/labra/ShExcala")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/labra/ShExcala"), "scm:git:git@github.com:labra/ShExcala.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://labra.github.io/ShExcala/latest/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>labra</id>
        <name>Jose Emilio Labra Gayo</name>
        <url>https://github.com/labra/</url>
      </developer>
    </developers>
  ),
  scalacOptions in (Compile,doc) ++= Seq(
//    "-Xfatal-warnings",
    "-diagrams-debug",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  )
)
