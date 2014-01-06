name := "ShExcala"

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.7" % "test" , 
  "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test"
  )

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)