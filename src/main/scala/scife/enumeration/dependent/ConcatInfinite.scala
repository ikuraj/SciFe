package scife.enumeration
package dependent

import scife.enumeration.lzy._

import scife.util._

class ConcatInfinite[I, O]
  (s1: DependInfinite[I, O], s2: DependInfinite[I, O])
  extends DependInfinite[I, O] with HasLogger {

  override type EnumType = Infinite[O]

  override def getEnum(parameter: I) = {
    val left = s1.getEnum(parameter)
    val right = s2.getEnum(parameter)

    ConcatInfinite(left, right)
  }

}
