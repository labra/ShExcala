addSbtPlugin("com.orrsella" % "sbt-sublime" % "1.1.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.4.0")

dependencyOverrides += "org.scala-sbt" % "sbt" % "0.13.8"

addSbtPlugin("com.lihaoyi" % "utest-js-plugin" % "0.1.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.7.0-RC2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.2.1")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

// addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.4.3")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.1")

resolvers += Classpaths.sbtPluginReleases

resolvers +=  Resolver.url(
  "scala-js-releases", url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns)
  
resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

