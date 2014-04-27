name := "ShExcala"

version := "1.0"

scalaVersion := "2.10.2"

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
 )

scalacOptions ++= Seq("-deprecation")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)





