name := "InSynth"

version := "1.3"

organization := "ch.epfl.lara"

scalaVersion := "2.10.3"

scalaBinaryVersion := "2.10"

//scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature") 
// for benchmarking
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xdisable-assertions")

scalacOptions ++= Seq("-Xelide-below", "OFF") 

javacOptions += "-Xlint:unchecked"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3" % "test"

// logging facilities
libraryDependencies ++= Seq(
  "com.typesafe" %% "scalalogging-log4j" % "1.0.1",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta3",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3"
)

seq(ScctPlugin.instrumentSettings : _*)

seq(com.github.theon.coveralls.CoverallsPlugin.coverallsSettings: _*)

libraryDependencies +=  "com.googlecode.kiama" %% "kiama" % "1.5.1"

libraryDependencies +=  "com.googlecode.kiama" %% "kiama" % "1.5.1" % "test" classifier ("test")

// ScalaCheck
resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

// micro-benchmarks
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.5-SNAPSHOT" % "test"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false