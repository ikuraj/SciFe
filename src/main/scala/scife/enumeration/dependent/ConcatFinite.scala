package scife.enumeration
package dependent

import scife.enumeration.lzy._

import scife.util._

class ConcatFinite[I, O]
  (s1: DependFinite[I, O], s2: DependFinite[I, O])
  extends DependFinite[I, O] with HasLogger {

  override type EnumSort[A] = Finite[A]

  override def getEnum(parameter: I) = {
    val left = s1.getEnum(parameter)
    val right = s2.getEnum(parameter)

    ConcatFinite(left, right)
  }

}
