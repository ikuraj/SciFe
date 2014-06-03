package insynth.enumeration
package reverse

import scala.reflect._
import scala.language.implicitConversions

// NOTE could explore solution with Invariant Reverse and an implicit
// conversion to its objects

trait Reverse[A] extends Enum[A] {
  
  def reverse(a: A): Int
 
}

trait ReverseFinite[A] extends Finite[A] with Reverse[A]

trait ReverseInfinite[A] extends Infinite[A] with Reverse[A]

object Reverser {

  def apply[T](arg: T, args: T*)(implicit ct: ClassTag[T]): Reverse[T] =
    fromFiniteCollection(arg :: args.toList)

  def apply[T](col: Traversable[T])(implicit ct: ClassTag[T]): Reverse[T] =
    fromFiniteCollection(col)

  private[enumeration] def fromFiniteCollection[I, T](col: Traversable[T])
    (implicit ct: ClassTag[T]): Reverse[T] =
    col match {
      case _ if col.size == 0 => new Empty
      case _ if col.size == 1 => Singleton(col.head)
      case _ => new WrapArray(col.toArray)
    }

//  def apply[T](enum: Enum[T], v: T) = {
//    enum
//  }
//  
//  def apply[T](enum: Finite[T], v: T) = enum match {
//    case e@Empty =>
//      e
//    case m: Map
//  override def reverse[V >: U](a: V) =
//    en.Map( enum.reverse( revFun( a.asInstanceOf[U] ) ), modify)
//  }

}