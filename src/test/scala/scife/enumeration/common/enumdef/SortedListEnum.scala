package scife
package enumeration
package common.enumdef

import scife.{ enumeration => e }
import scife.util._

import scala.language.postfixOps

object SortedListEnum {
  
  import dependent._
  import memoization._
  
  type EnumType = Depend[(Int, Int), List[Int]]

  def constructEnumerator(implicit ms: MemoizationScope) = {

    val naturals = Depend( (range: Int) => { e.WrapArray( 1 to range ) })

    Depend.memoized(
      ( self: EnumType, pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size == 0) e.Singleton( Nil )
        else if (size > 0) {
          val roots = naturals.getEnum(max)

          val innerLists: Depend[Int, List[Int]] = InMap(self, { (par: Int) =>
            (size - 1, par)
          })

          val allLists =
            memoization.Chain[Int, List[Int], List[Int]](roots, innerLists,
              (head: Int, l: List[Int]) => {
                head :: l
              }
            )

          allLists
        } else e.Empty
      }
    )
  }

  def constructEnumeratorStrict(implicit ms: MemoizationScope) = {

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
            memoization.Chain[Int, List[Int], List[Int]](roots, innerLists,
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
