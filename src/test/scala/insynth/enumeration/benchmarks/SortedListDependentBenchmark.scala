package insynth
package enumeration
package benchmarks

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import Structures.RedBlackTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

//package object RedBlackTreeDependentBenchmark {
//  type EnumType = Dependent[(Int, Range, Set[Boolean], Int), Tree]

class SortedListDependentBenchmark
  extends StructuresBenchmark[Depend[(Int, Int), List[Int]]]
	//extends DependentMemoizedBenchmark[Int, Depend[(Int, Int), List[Int]]]
  with java.io.Serializable with HasLogger {

  val maxSize = BenchmarkSuite.sizeSortedList

  override def name = "SortedList"

  //fixtureRun("strictly", constructEnumerator = (ms: MemoizationScope) => constructEnumerator(ms))
  fixture(constructEnumerator = (ms: MemoizationScope) => constructEnumerator(ms))

  type EnumType = Depend[(Int, Int), List[Int]]

  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
    using in { (size: Int) =>
      val enum = tdEnum.getEnum((size, size))
      for ( ind <- 0 until enum.size ) enum(ind)
    }
  }

//  def generator = Gen.range("size")(1, maxSize, 1)

  def warmUp(inEnum: EnumType) {
    val tdEnum = inEnum.asInstanceOf[EnumType]
    for (size <- 1 to maxSize) {
      val enum= tdEnum.getEnum((size, size))
      val elements =
        for (
          ind <- 0 until enum.size
        ) enum(ind)
    }
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {
    
    val naturals = Depend.memoized( (range: Int) => { e.Enum( 1 to range ) })
    
    Depend.memoized(
      ( self: EnumType, pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size > max) e.Empty
        else
        if (size == 0) e.Singleton( Nil )
        else /*if (size > 0)*/ {
          val roots = naturals.getEnum(max)
          
          val innerLists: Depend[Int, List[Int]] = InMap(self, { (par: Int) =>
            (size - 1, par)
          })
          
          val allLists =
            if (size < 10)
              memoization.Chain[Int, List[Int], List[Int]](roots, innerLists,
                (head: Int, l: List[Int]) => {
                  head :: l
                }
              )
            else
              e.dependent.Chain[Int, List[Int], List[Int]](roots, innerLists,
                (head: Int, l: List[Int]) => {
                  head :: l
                }
              )
          
          allLists
        }// else e.Empty
      }
    )
  }

  def constructEnumeratorStrict(ms: MemoizationScope) = {
    
    val naturals = Depend((range: Int) => { e.WrapArray( 1 to range ) })
    
    Depend.memoized(
      ( self: EnumType, pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size == 0) e.Singleton( Nil )
        else if (size > 0) {
          val roots = naturals.getEnum(max)
          
          val innerLists: Depend[Int, List[Int]] = InMap(self, { (par: Int) =>
            (size - 1, par - 1)
          })
          
          val allLists =
            if (size < 5)
              memoization.Chain[Int, List[Int], List[Int]](roots, innerLists,
                (head: Int, l: List[Int]) => {
                  head :: l
                }
              )
            else
              e.dependent.Chain[Int, List[Int], List[Int]](roots, innerLists,
                (head: Int, l: List[Int]) => {
                  head :: l
                }
              )
          
          allLists
        } else e.Empty
      }
    )
  }

}
//}
