package scife.enumeration
package memoization

import scala.collection.mutable._

import scife.util._

import scala.reflect._

// TODO maybe later
trait MemoizedStaticBoolSet[T] extends Enum[T] with Memoizable with HasLogger {
  
  implicit val classTagT: ClassTag[T]// = implicitly[ClassTag[T]]
  assert(classTagT != null)

  private[this] var memoizedValues = new Array[T](this.size)
  private[this] var memoizedFlags = new BitSet()
  
  def isMemoized(ind: Int) = memoizedFlags(ind)

  override abstract def apply(ind: Int) = {
    assert(this.size > ind)
    if (!memoizedFlags(ind)) {
      memoizedValues(ind) = super.apply(ind)
      memoizedFlags(ind) = true
    }

    memoizedValues(ind)
  }

  def getMemoizedFlags = this.memoizedFlags
  def getMemoizedValues = this.memoizedValues
  
  def merge(other: MemoizedStaticBoolSet[T]) = {
    for (ind <- other.getMemoizedFlags.diff(memoizedFlags)) {
      memoizedValues(ind) = other.getMemoizedValues(ind)
      memoizedFlags += ind
    }
  }

  override def clearMemoization {
    memoizedValues = new Array[T](this.size)
    memoizedFlags.clear
  }

}