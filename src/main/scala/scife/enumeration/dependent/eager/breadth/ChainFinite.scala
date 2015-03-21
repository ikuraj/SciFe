package scife.enumeration
package dependent
package breadth

import combinators._
import scife.{ enumeration => e }

import scife.util._

class ChainFiniteCombine[I, O, R]
  (s1: Finite[I], s2: DependFinite[I, O], combine: (I, O) => R)
  extends Finite[R] with HasLogger {

  val rr = {
    val streams =
      for (ind <- 0 until s1.size; leftProduced = s1(ind); rightStream = s2.getEnum( leftProduced );
        if rightStream.size > 0 ) yield {
          e.Map( rightStream, { (rightProduced: O) => combine(leftProduced, rightProduced) })
        }

    e.lzy.breadth.ConcatFinite[R]( streams )
  }

  override def size = rr.size

  override def apply(ind: Int) =
    rr(ind)

}