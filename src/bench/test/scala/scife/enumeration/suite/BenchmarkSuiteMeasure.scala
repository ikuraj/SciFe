package scife.enumeration
package suite

import benchmarks._

import org.scalameter._

import reporting._
import execution._
import Key._

class BenchmarkSuiteMeasure extends scife.enumeration.util.Benchmarker {

  import benchmarks.measures.enumdef._
  
//  val numberOfJVMs = System.getenv("NumOfJVMs")
//  
//  println(System.getenv("NumOfJVMs"))
//  println(System.getProperty("NumOfJVMs"))
  
  import common.enumdef.BinarySearchTreeEnum._
  
  implicit val listToTest = { 
    val enumMap = (enumDefList.map(_._2) zip enumDefList).toMap
    List(
      "constructEnumeratorBenchmarkNoTuplesWhenConstructingTree",
      "constructEnumeratorBenchmark"
    ).map(enumMap(_))
  }
  
  new BinarySearchTreeEnum().getMeasurements(15,
    BenchmarkSuite.configArgumentsFull// + (exec.maxWarmupRuns -> 2)
  )
    
  override val executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default)
  
}
