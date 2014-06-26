package insynth.enumeration.member
package benchmarks

import org.scalameter._

import reporting._
import execution._
import Key._

package suite {
  // if set, does not run full-blown micro-benchmark test suite; it runs
  // a quicker benchmark with less reliable results
  
  class BenchmarkSuiteMinimal extends PerformanceTest.OfflineReport {    
    override def persistor = new persistence.SerializationPersistor
        
    import BenchmarkSuite._
    
    implicit val configArguments = 
      org.scalameter.Context(
        exec.maxWarmupRuns -> 2,
        exec.benchRuns -> 3, 
        exec.independentSamples -> 1
      )
    
    for( ((benchmark, name), maxSize) <- allBenchmarks zip minimalSizes)
      benchmark.fixture("Minimal benchmarks", name, maxSize)
          
  }

  class BenchmarkSuiteFull extends PerformanceTest.OfflineReport {    
    import BenchmarkSuite._
    
    implicit val configArguments = 
      org.scalameter.Context(
        exec.maxWarmupRuns -> warmUps,
        exec.benchRuns -> numberOfRuns, 
        exec.independentSamples -> JVMs,
        exec.jvmcmd -> javaCommand,
        exec.jvmflags -> JVMFlags.mkString(" ")
      )
    
    for( ((benchmark, name), maxSize) <- allBenchmarks zip fullBlownSizes)
      benchmark.fixtureRun(benchmarkMainName, "SciFe", maxSize, name)
      
  }
  
}

object BenchmarkSuite {
  
  val benchmarkMainName = "Benchmarks"

  val allBenchmarks = List(
    (new RedBlackTreeBenchmarkMember, "Red-Black Trees, membership"),
    (new RedBlackTreeBenchmarkVerify, "Red-Black Trees, invariant execution")
  )
    
  val minimalSizes = List(3, 3)

  val fullBlownSizes = List(15, 15)
  val warmUps = 8
  val numberOfRuns = 3
  val JVMs = 3

//  val fullBlownSizes = List(3, 3, 3, 3, 3)
//  val warmUps = 1; val numberOfRuns = 3; val JVMs = 1

  lazy val javaCommand = "java -server"
  lazy val JVMFlags = List(
    // print important outputs
//    "-XX:+PrintCompilation", "-verbose:gc",
    // compilation
//    "-Xbatch", "--XX:CICompilerCount=1",
//    // optimizations
    "-XX:ReservedCodeCacheSize=512M",
    "-XX:CompileThreshold=100", "-XX:+TieredCompilation",
    "-XX:+AggressiveOpts", "-XX:MaxInlineSize=512",
    // memory
    "-Xms44G", "-Xmx44G"
  )
//  println("JVM FLags: " + JVMFlags.mkString(" "))
  
}
