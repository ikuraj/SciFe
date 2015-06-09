package scife
package enumeration
package benchmarks

import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import structures.RedBlackTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class RedBlackTreeDependentBenchmark
  extends StructuresBenchmark[Depend[(Int, Range, Set[Boolean], Int), Tree]]
  with java.io.Serializable {

  type EnumType = Depend[(Int, Range, Set[Boolean], Int), Tree]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      for (
        blackHeight <- blackHeightRange(size);
        enum = tdEnum.getEnum(size, 1 to size, Set(true, false), blackHeight);
        ind <- 0 until enum.size
      ) enum(ind)
    }
  }

  def warmUp(inEnum: EnumType, maxSize: Int) {
    val tdEnum = inEnum.asInstanceOf[Depend[(Int, Range, Set[Boolean], Int), Tree]]
    for (size <- 1 to maxSize) {
      val tdEnumVal = tdEnum
      for (
        blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
        enum = tdEnum.getEnum(size, 1 to size, Set(true, false), blackHeight);
        ind <- 0 until enum.size
      ) enum(ind)
    }
  }
  
  def constructEnumerator(implicit ms: MemoizationScope) =
    e.common.enumdef.RedBlackTreeEnum.constructEnumerator_currentBenchmark(ms)

}
