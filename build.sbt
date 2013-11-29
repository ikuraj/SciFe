name := "InSynth"

version := "1.3"

organization := "ch.epfl.lara"

scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions += "-Xlint:unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

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