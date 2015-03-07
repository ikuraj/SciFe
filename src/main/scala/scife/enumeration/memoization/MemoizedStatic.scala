package scife.enumeration
package memoization

import scala.collection.mutable._

import scife.util._

import scala.reflect._

// TODO maybe later
trait MemoizedStatic[T] extends Enum[T] with Memoizable with HasLogger {
  
  implicit val classTagT: ClassTag[T]// = implicitly[ClassTag[T]]
  assert(classTagT != null)

  private[this] var memoizedValues = new Array[T](this.size)
  private[this] var memoizedFlags = new Array[Boolean](this.size)
  
  def isMemoized(ind: Int) = memoizedFlags(ind)

  override abstract def apply(ind: Int) = {
    assert(this.size > ind)
    if (!memoizedFlags(ind)) {
      memoizedFlags(ind) = true
      memoizedValues(ind) = super.apply(ind)
    }

    memoizedValues(ind)
  }

  override def clearMemoization {
    memoizedValues = new Array[T](this.size)
    memoizedFlags = new Array[Boolean](this.size)
  }

}

// TODO maybe later
trait MemoizedStaticNull[T >: Null] extends Enum[T] with Memoizable with HasLogger {
  
  implicit def classTagT: ClassTag[T]

  protected[enumeration] val memoizedValues = new Array[T](this.size)

  override abstract def apply(ind: Int) = {
    if (memoizedValues(ind) == null)
      memoizedValues(ind) = super.apply(ind)

    memoizedValues(ind)
  }

  override def clearMemoization {
    for (ind <- 0 until memoizedValues.size)
      memoizedValues(ind) = null
  }

}
