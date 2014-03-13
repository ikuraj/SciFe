//package insynth
//package streams
//package benchmarks
//
//import org.scalameter.api._
//import util.logging._
//
//import dependent._
//import streams.{ light => e }
//import util.Structures.RedBlackTrees._
//
//	class BBBenchmark extends DependentMemoizedBenchmark[Int, Dependent[Int, Int]] with HasLogger {  
//	
//	  def name = "Red Black Tree"
//	  
//	  def measureCode(using: Using[Int], tdEnum: Dependent[Int, Int]) {
//	  }
//	  
//	  def generator = Gen.range("size")(1, 15, 1)
//	  
//	  def warmUp(inEnum: Dependent[Int, Int]) {    
//	  }
//	  
//	  def constructEnumerator(implicit ms: MemoizationScope) = {
//	    null
//	  }
//	
//	}