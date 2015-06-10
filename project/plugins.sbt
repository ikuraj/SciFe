// coverage
resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.0.0")

// scalastyle
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.5.0")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
