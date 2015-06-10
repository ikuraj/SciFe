import sbt._
import Keys._

import scoverage._

object SciFeBuild extends Build {
  
  val preferredJVM = Some("jvm-1.7")
  
  lazy val root =
    Project("SciFe", file("."))
      .configs( BenchConfig )
      .settings( inConfig(BenchConfig)(Defaults.testTasks): _*)
      .settings(
        // add commands
        commands ++= Seq(benchCommand, benchBadgeCommand),
        // fork by default,
//        fork := false,

        // test options 
        fork in Test := true,
        javaOptions in Test ++= Seq("-Xms2048m", "-Xmx2048m",
          "-XX:MaxPermSize=512m", "-XX:+UseConcMarkSweepGC"),
        // verbose QuickCheck error ouput
        testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3"),
        // exclude slow tests
        testOptions in Test += noSlowTests,
        
        // benchmark options
        //unmanagedSourceDirectories in BenchConfig <+= sourceDirectory ( _ / "bench" ),
        unmanagedSourceDirectories in Test <+= sourceDirectory ( _ / "bench" ),
        // run only benchmark not dependent tests
        sourceDirectories in compile in BenchConfig <+= sourceDirectory ( _ / "bench" ),
        //sources in (BenchConfig, test) := Seq ( sourceDirectory.value / "bench" ),
        fork in BenchConfig := false,        
        includeFilter in BenchConfig := AllPassFilter,
        testOptions in BenchConfig := Seq( benchmarksFilter/*, noSuiteFilter */ ),
//        testOptions in BenchConfig += Tests.Filter({ (s: String) =>
//          val isFull = s endsWith "Full"
//          !isFull
//        }),
        scalacOptions in BenchConfig ++= generalScalacFlagList,
        scalacOptions in BenchConfig ++= optimizedCompileScalacFlagsList,
        
        // ScalaMeter
        parallelExecution in BenchConfig := false,
        testFrameworks in BenchConfig += new TestFramework("org.scalameter.ScalaMeterFramework")
        
        // Scoverage
        , ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages :=
          "<empty>;scife\\.util\\.*;scife\\.enumeration\\.util.*"+
          ";scife\\.util\\.format\\.*;scife\\.util\\.logging\\.*"
      )

  val benchRegEx = //"""(.*\.suite\.[^\.]*Suite*)"""
    """(.*\.benchmarks\..*)"""
  val benchmarksFilter = Tests.Filter(
    _ matches """(.*\.benchmarks\..*)"""
  )
  val noSuiteFilter = Tests.Filter(
    (s: String) => !(s matches """.*Suite.*""")
  )
  val noSlowTests = Tests.Argument(TestFrameworks.ScalaTest, "-l", "tags.Slow")
  
  lazy val BenchConfig = config("benchmark") extend(Test)
    
  def benchCommand = Command.single("bench") { (state, arg) =>
    
    val extracted: Extracted = Project.extract(state)
    import extracted._
    
    arg match {
      case "full" =>
        val fullState =
          append(Seq(
            testOptions in BenchConfig := (testOptions in BenchConfig).value diff Seq(noSuiteFilter),
            testOptions in BenchConfig += Tests.Filter(_ endsWith "Full")), state)
        Project.runTask(test in BenchConfig, fullState)
        // return the same state (not the modified one with filters)
        state
      case "minimal" | "simple" =>
        val minState =          
          append(Seq(
            testOptions in BenchConfig := (testOptions in BenchConfig).value diff Seq(noSuiteFilter),
            testOptions in BenchConfig += Tests.Filter(_ endsWith "Minimal")
          ), state)
        Project.runTask(test in BenchConfig, minState)
        state
      case "measure" =>
        val measureState =          
          append(Seq(
            testOptions in BenchConfig := (testOptions in BenchConfig).value diff Seq(noSuiteFilter),
            testOptions in BenchConfig += Tests.Filter(_ endsWith "Measure")
          ), state)
        Project.runTask(test in BenchConfig, measureState)
        state
      case "profile" =>
        val profileState =          
          append(Seq(
            testOptions in BenchConfig := (testOptions in Test).value diff Seq(noSlowTests),
            testOptions in BenchConfig += Tests.Filter(_ contains "Profile"),
            // for one JVM
            fork in test in BenchConfig  := true,
            javaOptions in test in BenchConfig := profileJVMFlagList ++ remoteConnectionJVMFlagList
          ), state)
        Project.runTask(test in BenchConfig, profileState)
        state
      case "slow" =>
        val slowState =          
          append(Seq(testOptions in BenchConfig += Tests.Filter(_ endsWith "Slow")), state)
        Project.runTask(test in BenchConfig, slowState)
        slowState
      case "debug" =>
        Project.runTask(test in BenchConfig, state)
        state
      case _ =>
        state.fail
    }
  }
  
  val profileJVMFlagList = List(
    // print important outputs
//    "-XX:+PrintCompilation",
    // verbose GC
    "-verbose:gc", "-XX:+PrintGCTimeStamps", "-XX:+PrintGCDetails",
    // compilation
    "-Xbatch",
//    "--XX:CICompilerCount=1",
    // optimizations
    "-XX:ReservedCodeCacheSize=512M",
    "-XX:CompileThreshold=100", "-XX:+TieredCompilation",
    "-XX:+AggressiveOpts", "-XX:MaxInlineSize=512"
    ,
    // memory
    "-Xms32G", "-Xmx32G"
    // new generation size
//    ,"-XX:NewSize=20G",
//    // disable adaptive policy
//    "-XX:-UseAdaptiveSizePolicy",
//    "-XX:MinHeapFreeRatio=100",
//    "-XX:MaxHeapFreeRatio=100"
  )
  
  val remoteConnectionJVMFlagList = List(
    "-Dcom.sun.management.jmxremote",
    "-Dcom.sun.management.jmxremote.port=4567",
    "-Dcom.sun.management.jmxremote.ssl=false",
    "-Dcom.sun.management.jmxremote.authenticate=false",
    "-Djava.rmi.server.hostname=128.52.186.35"
  )
    
  val generalScalacFlagList = List(
    "-deprecation", "-unchecked", "-feature",
    // no debugging info
    "-g", "none"
  ) ::: preferredJVM.toList.flatMap( "-target " :: _ :: Nil )
  
  val optimizedCompileScalacFlagsList = List(
    "-Xdisable-assertions",
    // elide logging facilities
    "-Xelide-below", "OFF",
    // group of flags for optimization
    "-optimise",
    "-Yinline"
  )
  
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
