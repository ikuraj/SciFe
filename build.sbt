name := "SciFe"

version := "1.2.0"

organization := "ch.epfl.lara"

organizationName := "LARA/EPFL"

organizationHomepage := Some(new URL("http://lara.epfl.ch"))

scalaVersion := "2.11.4"

scalaBinaryVersion := "2.11"

// Compiler options
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"/*, "-Ylog-classpath" */) 

javacOptions += "-Xlint:unchecked"

// Testing libraries

// ScalaCheck
resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.12.1" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
//libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

// enables running JUnit tests from sbt
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

// ScalaLogging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.7"
//libraryDependencies += "com.typesafe" %% "scalalogging-log4j" % "1.0.1"

libraryDependencies ++= Seq(
  "org.apache.logging.log4j" % "log4j-api" % "2.0.2",
  "org.apache.logging.log4j" % "log4j-core" % "2.0.2",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.0.2"
)

// ScalaMeter
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"

// Kiama attribute grammars
libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.7.0"

// Coverage
instrumentSettings

CoverallsPlugin.coverallsSettings

ScoverageKeys.excludedPackages in ScoverageCompile := "<empty>;insynth.util.*"

// Check style
org.scalastyle.sbt.ScalastylePlugin.Settings
