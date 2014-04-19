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
  
  val sizeBinarySearchTree = 14
  val sizeHeapArray = 10
  val sizeRedBlackTree = 15//20
  val sizeSortedList = 14//16
  
}
