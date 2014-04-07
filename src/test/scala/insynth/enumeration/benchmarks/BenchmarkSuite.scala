package insynth.streams.benchmarks

import org.scalatest._
import org.scalameter._

import reporting._
import Key._

class BenchmarkSuite extends PerformanceTest.Regression { 
  def persistor = Persistor.None//new persistence.SerializationPersistor

  include[SortedListDependentBenchmark]
  include[RedBlackTreeDependentBenchmark]
  include[BinarySearchTreeBenchmark]
}