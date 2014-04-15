package insynth.enumeration.benchmarks

import org.scalatest._
import org.scalameter._

import reporting._
import Key._

package suite {
  class BenchmarkSuite extends PerformanceTest.OfflineRegressionReport { 
    override def persistor = new persistence.SerializationPersistor
  
    include[SortedListDependentBenchmark]
    include[RedBlackTreeDependentBenchmark]
    include[BinarySearchTreeBenchmark]
    include[HeapArrayBenchmark]
  }
}

object BenchmarkSuite { 
  
  var maxSize = 15
  
}