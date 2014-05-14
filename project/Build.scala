import sbt._
import sbt.Keys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import Def.ScopedKey
import scala.scalajs.sbtplugin.ScalaJSPlugin._

/**
 * this files is intended to build the main project
 * it contains links to all dependencies that are needed
 * */
object Build extends sbt.Build {

  val isRelease = true 
 
  def repo = "weso-releases" 

  val shExcalaVersion = "0.0.1"
    
  publishMavenStyle := false

  val currentVersion = shExcalaVersion

  protected val bintrayPublishIvyStyle = settingKey[Boolean]("=== !publishMavenStyle") //workaround for sbt-bintray bug
  
  lazy val publishSettings = Seq(
    repository in bintray := this.repo,
    bintrayOrganization in bintray := Some("weso"),
    licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0")),
    bintrayPublishIvyStyle := true
  )


  /**
   * For parts of the project that we will not publish
   */
  lazy val noPublishSettings = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  val sharedSettings = Seq(
      organization := "es.weso",
      name := "shExcala",
      scalaVersion := "2.10.4"
    )

  val scalajsResolver: URLRepository = Resolver.url("scala-js-releases",
    url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
      Resolver.ivyStylePatterns)

  
/*  val wesinDeps = Seq(
    "org.scalatest" % "scalatest_2.10" % "2.1.3",
    "com.assembla.scala-incubator" % "graph-core_2.10" % "1.7.3", //TODO: fix errors in tgraphimpl to migrate to 1.8.0
    "org.apache.commons" % "commons-lang3" % "3.1"
  )

  val wesinResolvers = Seq(
    "namin.github.com/maven-repository" at "http://namin.github.com/maven-repository/",
    "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
  )

  lazy val wesinSettings =
    Seq(
      name := "wesin",
      scalacOptions ++= Seq( "-feature", "-language:_" ),
      version := "0.1.0",
      scalaVersion := "2.10.3",
      resolvers ++= wesinResolvers,
      libraryDependencies ++=wesinDeps,
      organization := "es.weso",
      licenses += ("MPL", url("http://opensource.org/licenses/MPL-2.0"))
    ) ++ bintraySettings ++ publishSettings


  lazy val wesin  = Project(
    id   = "wesin",
    base = file(".")
  ) settings (wesinSettings: _*)
*/


} // needed for custom scalastyle package
