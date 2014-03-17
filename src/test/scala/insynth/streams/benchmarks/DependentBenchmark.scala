package insynth
package streams
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import streams.{ light => e }

import util.logging._

trait DependentMemoizedBenchmark[I, DepEnumType] extends
	PerformanceTest.Quickbenchmark with java.io.Serializable with HasLogger {
  import util.Structures._

  val benchmarkMainName = "SciFe_Dependent_Enumerators"
    
  val JVMParameters =
    // important outputs
    "-XX:+PrintCompilation, -verbose:gc" +
    // compilation
    "-Xbatch, --XX:CICompilerCount=1"
  
  def fixture: Unit = fixture()
  
  def fixtureRun(
    run: String,
		constructEnumerator: MemoizationScope => DepEnumType = this.constructEnumerator,
    generator: Gen[I] = this.generator,
    warmUp: DepEnumType => Any = this.warmUp,
    measureCode: (super.Using[I], DepEnumType) => Any = this.measureCode,
    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUp
  ) =
	  performance of benchmarkMainName in {
	    performance of name in {
    		measure method run in {
		      implicit val memScope = new MemoizationScope
		      val enumerator = constructEnumerator(memScope)
		
		      measureCode(
		        using(generator) curve (name) warmUp {
			        warmUp(enumerator)
			      } setUp {
			        setUpFixed(_, enumerator, memScope)
			      }, enumerator
		      )
    		}    
	    }
  	}

  def fixture(
		constructEnumerator: MemoizationScope => DepEnumType = this.constructEnumerator,
    generator: Gen[I] = this.generator,
    warmUp: DepEnumType => Any = this.warmUp,
    measureCode: (super.Using[I], DepEnumType) => Any = this.measureCode,
    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUp
  ) =
	  performance of benchmarkMainName in {
	    performance of name in {
	      implicit val memScope = new MemoizationScope
	      val enumerator = constructEnumerator(memScope)
	
	      measureCode(
	        using(generator) curve (name) warmUp {
		        warmUp(enumerator)
		      } setUp {
		        setUpFixed(_, enumerator, memScope)
		      }, enumerator
	      )
	    }
  	}

	def measureCode(using: super.Using[I], tdEnum: DepEnumType): Any
  
  def name: String
  
  def generator: Gen[I]
  
  def warmUp(tdEnum: DepEnumType): Any
  
  def setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {}
  
  final def setUpFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope)
    System.gc
    memScope.clear
  }

  def constructEnumerator(ms: MemoizationScope): DepEnumType

}