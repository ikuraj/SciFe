package scife
package enumeration
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

trait StructuresBenchmark[DepEnumType] extends DependentMemoizedBenchmark[Int, DepEnumType] {

  override def generator(maxSize: Int): Gen[Int] =
    Gen.range("size")(1, maxSize, 1)
//    Gen.range("size")(maxSize, maxSize, 1)

  override def setUp(i: Int, tdEnum: DepEnumType, memScope: MemoizationScope) =
    memScope.clear
}
