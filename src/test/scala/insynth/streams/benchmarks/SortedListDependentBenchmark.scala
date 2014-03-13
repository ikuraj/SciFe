package insynth
package streams
package benchmarks

import org.scalameter.api._
import util.logging._

import dependent._
import streams.{ light => e }
import util.Structures.RedBlackTrees._

//package object RedBlackTreeDependentBenchmark {
//  type EnumType = Dependent[(Int, Range, Set[Boolean], Int), Tree]

class SortedListDependentBenchmark extends DependentMemoizedBenchmark[Int, Dependent[(Int, Int), List[Int]]]
  with java.io.Serializable with HasLogger {

  val maxSize = 2

  fixtureRun("strictly", constructEnumerator = constructEnumeratorStrict)
  fixtureRun("equal", constructEnumerator = constructEnumerator)

  type EnumType = Dependent[(Int, Int), List[Int]]

  override val name = "Sorted List"

  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
    using in { (size: Int) =>
      val enum = tdEnum.getStream((size, size))
      val elements =
        for ( ind <- 0 until enum.size ) yield enum(ind)
    }
  }

  def generator = Gen.range("size")(1, maxSize, 1)

  def warmUp(inEnum: EnumType) {
    val tdEnum = inEnum.asInstanceOf[EnumType]
    for (size <- 1 to maxSize) {
      val enum= tdEnum.getStream((size, size))
      val elements =
        for (
          ind <- 0 until enum.size
        ) yield enum(ind)
    }
  }

  def constructEnumerator(ms: MemoizationScope) = {
    
    val naturals = Producer[Int, Int]( (range: Int) => { e.WrapperArray( 1 to range ) })
    
    Producer.memoized(
      ( self: EnumType, pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size == 0) e.Singleton( Nil )
        else if (size > 0) {
          val roots = naturals.getStream(max)
          
          val innerLists: Dependent[Int, List[Int]] = new InMapper(self, { (par: Int) =>
            (size - 1, par)
          })
          
          val allLists =
            BinaryFiniteMemoized.combine[Int, List[Int], List[Int]](roots, innerLists,
              (head: Int, l: List[Int]) => {
                head :: l
              }
            )
          
          allLists
        } else e.Empty
      }
    )
  }

  def constructEnumeratorStrict(ms: MemoizationScope) = {
    
    val naturals = Producer[Int, Int]( (range: Int) => { e.WrapperArray( 1 to range ) })
    
    Producer.memoized(
      ( self: EnumType, pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size == 0) e.Singleton( Nil )
        else if (size > 0) {
          val roots = naturals.getStream(max)
          
          val innerLists: Dependent[Int, List[Int]] = new InMapper(self, { (par: Int) =>
            (size - 1, par - 1)
          })
          
          val allLists =
            BinaryFiniteMemoized.combine[Int, List[Int], List[Int]](roots, innerLists,
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