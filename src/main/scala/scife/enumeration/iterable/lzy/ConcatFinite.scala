package scife.enumeration
package iterable
package lzy

import combinators._

import scala.collection.mutable
import scala.reflect.ClassTag

import scife.util._

import scala.language.higherKinds

class ConcatFiniteVariedSize[@specialized T, E[A] <: Finite[A]] protected[enumeration] (val enumArray: Array[E[T]])
  extends Finite[T] with ConcatMul[T] with HasLogger {

  override def enums = enumArray.toSeq

  override def apply(ind: Int) = {
    entering("apply", ind)
    val arrInd = binarySearch(ind)
    val elInd = ind - limits(arrInd)
    exiting("apply", enumArray(arrInd)(elInd))
  }

  private[this] val limits = {
    var _size = 0
    val ab = mutable.ArrayBuffer(0)
    for (stream <- enumArray) {
      _size += stream.size
      ab += _size
    }
    ab.toArray
  }

  override def size = {
    fine("limits = " + limits.mkString(","))
    limits.apply(enumArray.length)
  }

  private[enumeration] def binarySearch(target: Int): Int = {
    var left = 0
    // limits are indexed 0..length
    var right = enumArray.length
    while (left <= right) {
      val mid = (left + right) / 2
      fine("target=%d, left=%d, mid=%d, right=%d".format(target, left, mid, right))
      if (limits(mid) <= target && limits(mid + 1) > target)
        return mid
      else if (limits(mid) > target)
        right = mid
      else
        left = mid
    }
    // should not happen
    throw new RuntimeException
  }

}