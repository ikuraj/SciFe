import sbt._
import Keys._

// Scala code coverage
import scoverage.ScoverageSbtPlugin.instrumentSettings
import org.scoverage.coveralls.CoverallsPlugin.coverallsSettings

object SciFeBuild extends Build {
  lazy val root =
    Project("SciFe", file("."))
      .configs( BenchConfig )
      .settings( inConfig(BenchConfig)(Defaults.testTasks): _*)
      .settings(
        // add commands
        commands ++= Seq(benchCommand, benchBadgeCommand),
        
        // test options        
        fork in Test := true,
        javaOptions in Test += "-Xmx2048m",
        // exclude slow tests
        testOptions in Test += Tests.Argument("-l", "tags.Slow"),
        
        // benchmark options        
        unmanagedSourceDirectories in Test <+= sourceDirectory ( _ / "bench" ),
        // run only benchmark not dependent tests
        sourceDirectories in test in BenchConfig := Seq ( sourceDirectory.value / "bench" ),
        fork in BenchConfig := false,        
        testOptions in BenchConfig := Seq(Tests.Filter(benchFilter)),
        scalacOptions in BenchConfig ++= Seq("-deprecation", "-unchecked", "-feature", "-Xdisable-assertions"),
        scalacOptions in BenchConfig ++= Seq("-Xelide-below", "OFF")
        ,

        // ScalaMeter
        parallelExecution in BenchConfig := false,
        testFrameworks in BenchConfig += new TestFramework("org.scalameter.ScalaMeterFramework")
        
      )

  val benchRegEx = """(.*\.enumeration\.benchmarks\..*)"""
      
  def benchFilter(name: String): Boolean = {
    name matches benchRegEx
  }
  
  lazy val BenchConfig = config("benchmark") extend(Test)
    
  def benchCommand = Command.single("bench") { (state, arg) =>
    val extracted: Extracted = Project.extract(state)
    import extracted._

    arg match {
      case "full" =>
        val fullState =
          append(Seq(testOptions in BenchConfig := Seq(Tests.Filter(_ endsWith "Full")) ), state)
        Project.runTask(test in BenchConfig, fullState)
        // return the same state (not the modified one with filters)
        state
      case "minimal" | "simple" =>
        val minState =          
          append(Seq(testOptions in BenchConfig := Seq(Tests.Filter(_ endsWith "Minimal")) ), state)
        Project.runTask(test in BenchConfig, minState)
        state
      case "slow" =>
        val slowState =          
          append(Seq(testOptions in BenchConfig := Seq(Tests.Filter(_ endsWith "Slow")) ), state)
        Project.runTask(test in BenchConfig, slowState)
        state
      case "debug" =>
        Project.runTask(test in BenchConfig, state)
        state
      case _ =>
        state.fail
    }
  }
  
  import java.util._
  import java.text._
  
  val badgesUrl = "http://img.shields.io/badge/"
  val pattern = "benchmark-%s-green.svg"
  val downloadCommand = "wget -O ./tmp/status.svg %s%s"
  val suffixPattern = "benchmark-%s-green.svg"
    
  def benchBadgeCommand = Command.command("bench-badge") { state =>
    val currentTime = Calendar.getInstance().getTime()
    val dateFormat = new SimpleDateFormat("""dd%2'F'MM%2'F'yy""")
    
    val dateString = dateFormat format currentTime
    val suffix = suffixPattern format dateString
    
    val commandResult =
      Process(downloadCommand.format(badgesUrl, suffix)).lines
    
    println(commandResult)
    
    state
  }
  
}
