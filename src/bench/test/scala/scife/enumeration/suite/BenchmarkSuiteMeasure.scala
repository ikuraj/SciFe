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
  
  new BinarySearchTreeEnum().getMeasurements(15,
    BenchmarkSuite.configArgumentsFull )
    
  override val executor = LocalExecutor(
    Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default)
  
}