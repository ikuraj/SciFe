package scife
package enumeration
package benchmarks

import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import Structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class DAGStructureBenchmark
  extends StructuresBenchmark[Depend[
    //(Int, List[Int], List[Int], List[Int]), List[(List[Int], List[Int], List[Int])]
    (Int, Int), List[List[Int]]
  ]] {

  // (size, #class)
  type Input = (Int, Int)
  // list of extends
  type Output = List[List[Int]]
  type EnumType = Depend[Input, Output]

    import e.Enum

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      val enum = tdEnum.getEnum((size, 0))

      for ( ind <- 0 until enum.size ) enum(ind)
    }
  }

  def warmUp(tdEnum: EnumType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = tdEnum.getEnum((size, 0))

      for ( ind <- 0 until enum.size ) enum(ind)
    }
  }

  def constructEnumerator(implicit ms: MemoizationScope = null) = {

    val subListChooser: DependFinite[(Int, List[Int]), List[Int]] = Depend.fin(
      (self: DependFinite[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Nil): Finite[List[Int]]
        else if (size == 1) e.Enum(range map {List(_)}): Finite[List[Int]]
        else if (size <= range.size) {
          val temp = self.getEnum( (size - 1, range.tail) )
          val kept = Map( temp , { range.head :: (_: List[Int]) })
          val leftOut = self.getEnum( (size, range.tail) )

          val allNodes = e.Concat(kept, leftOut)
          allNodes: Finite[List[Int]]
        } else e.Empty: Finite[List[Int]]
      })

    val sublistForSizesUpTo: DependFinite[Int, List[Int]] = Depend.memoizedFin((classes: Int) => {
      val chain = memoization.Chain(
        (e.Map(e.Enum(1 to classes), { (el: Int) => (el, (1 to classes).toList ) } )): Finite[(Int, List[Int])],
        subListChooser: DependFinite[(Int, List[Int]), List[Int]]
      ): Finite[((Int, List[Int]), List[Int])]

      e.Concat(
        e.Singleton(Nil): Finite[List[Int]],
        Map(chain, { (r: ((Int, List[Int]), List[Int])) => r._2 })
      )}
    )

    Depend.memoized(
      (self: EnumType, par: Input) => {
      // list sorted descendingly
      implicit val (size, classes) = par

      if (size == 1) {
        sublistForSizesUpTo(classes) map { List(_) }
      }
      else {
        e.Product(
          sublistForSizesUpTo(classes),
          self(size - 1, classes + 1)
        ) map { case (h, tail) => h :: tail }
      }
    })
  }

}
