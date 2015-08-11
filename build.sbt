name := "SciFe"

version := "1.2.9"

organization := "ch.epfl.lara"

organizationName := "LARA/EPFL"

organizationHomepage := Some(new URL("http://lara.epfl.ch"))

scalaVersion := "2.11.4"

// forcing the version
ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

scalaBinaryVersion := "2.11"

// Compiler options
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"/*, "-Ylog-classpath" */) 

javacOptions += "-Xlint:unchecked"

// Testing libraries

// ScalaCheck
resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.12.1" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
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

// Check style
org.scalastyle.sbt.ScalastylePlugin.Settings

// Combinatorics with collections
libraryDependencies += "com.googlecode.combinatoricslib" % "combinatoricslib" % "2.1"

// Math
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.4.1"

// Graphs
//projectDependencies += RootProject(uri("git://github.com/colder/bonsai.git"))
libraryDependencies += "org.jgrapht" % "jgrapht" % "0.9.0"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.0"

//libraryDependencies += "org.jgrapht" % "jgrapht-ext" % "0.9.0"
//
//libraryDependencies += "org.jgrapht" % "jgrapht-dist" % "0.9.0"

// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

// Specs2
libraryDependencies += "org.specs2" %% "specs2-core" % "3.0" % "test"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// in-program compilation calls
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value