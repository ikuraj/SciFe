package scife.enumeration
package memoization

import scala.collection.mutable.BitSet
import java.util.ArrayList

import scife.util._

trait MemoizedDynamicFast[T] extends Enum[T] with Memoizable with HasLogger {

  protected[enumeration] val memoizedFlags = new BitSet()
  protected[enumeration] val memoizedValues = new ArrayList[T](this.size)

  override abstract def apply(ind: Int) = {
    if (memoizedFlags contains ind)
      memoizedValues.get(ind)
    else {
      val nextValue = super.apply(ind)
      memoizedFlags += ind
      for (i <- 1 to ind - memoizedValues.size + 1)
        memoizedValues.add(nextValue)
      nextValue
    }
  }

  override def clearMemoization {
    memoizedFlags.clear
    memoizedValues.clear
  }

}
