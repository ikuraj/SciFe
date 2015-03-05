package scife.enumeration
package memoization
package scope

import scala.collection.mutable.MutableList

class AccumulatingScope extends MemoizationScope {

  val memoizations = MutableList[Memoizable]()

  def add(m: Memoizable) = m +=: memoizations

  def clear = for (m <- memoizations) m.clearMemoization

}
