import sbt._
import Keys._

object SciFeBuild extends Build {
  lazy val root =
    Project("SciFe", file("."))
      .configs( Bench )
      .settings( inConfig(Bench)(Defaults.testTasks): _*)
      .settings(
        fork in Test := true,
        javaOptions in Test += "-Xmx2048m",
        unmanagedSourceDirectories in Test <+= sourceDirectory ( _ / "bench" ),

        parallelExecution in Bench := false,
        fork in Bench := false,
        testFrameworks in Bench += new TestFramework("org.scalameter.ScalaMeterFramework"),
        includeFilter in Bench := AllPassFilter,
        testOptions in Bench := Seq(Tests.Filter(benchFilter)),
        scalacOptions in Bench ++= Seq("-deprecation", "-unchecked", "-feature", "-Xdisable-assertions"),
        scalacOptions in Bench ++= Seq("-Xelide-below", "OFF") 
      )

  val benchRegEx = """(.*\.benchmarks\.[^\.]*Benchmark)"""
      
  def benchFilter(name: String): Boolean = {
    name matches benchRegEx
  }
  
  lazy val Bench = config("bench") extend(Test)
}
