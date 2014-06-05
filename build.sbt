name := "SciFe"

version := "1.0-SNAPSHOT"

organization := "ch.epfl.lara"

scalaVersion := "2.10.3"

scalaBinaryVersion := "2.10"

// Compiler options
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature") 

javacOptions += "-Xlint:unchecked"

// Test libraries
libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.4" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3" % "test"

// ScalaCheck
resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3"

// Logging
libraryDependencies ++= Seq(
  "com.typesafe" %% "scalalogging-log4j" % "1.0.1",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta3",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3"
)

// Benchmarking
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.5-SNAPSHOT" % "test"

// Attribute grammars
libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.1"

// Coverage
instrumentSettings

CoverallsPlugin.coverallsSettings

ScoverageKeys.excludedPackages in ScoverageCompile := "<empty>;insynth.util.*"