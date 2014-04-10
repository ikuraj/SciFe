package insynth.enumeration.benchmarks

import org.scalatest._
import org.scalameter._

import reporting._
import Key._

class BenchmarkSuite extends PerformanceTest.OnlineRegressionReport { 
//  def persistor = Persistor.None //new persistence.SerializationPersistor

  include[SortedListDependentBenchmark]
  include[RedBlackTreeDependentBenchmark]
  include[BinarySearchTreeBenchmark]
  include[HeapArrayBenchmark]
}

object BenchmarkSuite { 
  
  var maxSize = 3
  
}