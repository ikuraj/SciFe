package scife.enumeration

import eager._
import memoization._

object Filter {

  type FilterFunction[A] = A => Boolean
//  type FilterFunction[A] = PartialFunction[A, Boolean]

  def apply[T](enum: Enum[T], filter: FilterFunction[T]) =
    enum match {
      case f: Finite[T] => new SimpleFilter(f, filter) with Finite[T]
      case i: Infinite[T] => new SimpleFilter(i, filter) with Infinite[T]
    }

  def memoized[T](enum: Enum[T], filter: FilterFunction[T])(implicit ct: scala.reflect.ClassTag[T]) =
    enum match {
      case f: Finite[T] =>
        new SimpleFilter(f, filter) with Finite[T] with MemoizedStatic[T] with MemoizedSize {
          override val classTagT = ct
        }
      case i: Infinite[T] => new SimpleFilter(i, filter) with Infinite[T] with MemoizedDynamic[T] with MemoizedSize
    }

  def apply[T](enum: Finite[T], filter: FilterFunction[T]) =
    new SimpleFilter(enum, filter) with Finite[T]

  def memoized[T](enum: Finite[T], filter: FilterFunction[T])(implicit ct: scala.reflect.ClassTag[T]) =
    new SimpleFilter(enum, filter) with Finite[T] with MemoizedStatic[T] with MemoizedSize {
      override val classTagT = ct
    }

  def apply[T](enum: Infinite[T], filter: FilterFunction[T]) =
    new SimpleFilter(enum, filter) with Infinite[T]

}
