// coverage
resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "0.99.7.1")

addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "0.99.0")

// scalastyle
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.5.0")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
