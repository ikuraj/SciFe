package scife.enumeration
package iterable
package lzy

import scife.{enumeration => e}
import util.EnumStream

import scala.reflect._

import scala.language.implicitConversions
//import scala.language.higherKinds
//
//trait Enum[+A, E[X] <: Enum[X, E]] extends Serializable {
//  
//  self: E[A] =>

object Enum extends EnumLessPriority {

  /* Factory methods */

  // only sequences are accepted since enumerators enumerate in a defined ordering
//  def apply[T](arg1: T, args: T*)(implicit ct: ClassTag[T]): Finite[T] =
//    fromFiniteCollection( (arg1 :: args.toList).toArray )
//
//  def apply[T](arr: Array[T]): Finite[T] =
//    fromFiniteCollection( arr )
//
//  def apply[T](stream: List[T])(implicit ct: ClassTag[T]): Finite[T] =
//    fromFiniteCollection(stream)
//
//  def apply[T](stream: Seq[T])(implicit ct: ClassTag[T]): Enum[T] =
//    fromCollection(stream)

  def apply(range: Range) =
    if (range.start == 0) new IdentitySize(range.size) with ResetIter[Int] with Touchable[Int]
    else new WrapRange(range) with ResetIter[Int] with Touchable[Int]

//  def apply[T](f: Int => T): Infinite[T] =
//    // NOTE in Scala this wont work
////    if (f == Predef.identity[Int] _)
////      Identity
////    else
//      WrapFunction(f)

//  def identity = Identity

//  def applyNoTag[T](args: T*): Enum[T] =
//    fromCollectionNoTag(args)

  /* Implicit conversion methods */

//  implicit def rangeToEnum(range: Range): Finite[Int] =
//    apply(range)
//
//  implicit def colToEnum[T](col: Seq[T])(implicit ct: ClassTag[T]) =
//    fromCollection(col)
//
//  implicit def elemsToEnum[T](elems: T*)(implicit ct: ClassTag[T]) =
//    fromFiniteCollection(elems)
//
//  implicit def traverableToEnum[T <% Traversable[_]](el: T) =
//    fromCollectionNoTag(el): Enum[_]
//
//  // needs to declare more specific type, otherwise singletonToEnum kicks in
//  implicit def streamToEnum[T](stream: Stream[T]): Infinite[T] =
//    new WrapStream(stream)
////    fromCollectionNoTag(col)
//
//  implicit def traversableToEnum[T](col: Traversable[T]): Enum[T] =
//    fromCollectionNoTag(col)
//
//  implicit def enumToCollection[T](e: Enum[T]): Seq[T] =
//    if (e.hasDefiniteSize)
//      e.toList
//    else EnumStream(e)
//
//  private[enumeration] def fromCollection[T](col: Traversable[T])(implicit ct: ClassTag[T]): Enum[T] =
//    col match {
//      case (stream: Stream[T]) if !stream.hasDefiniteSize => new WrapStream(stream)
//      case _ => fromFiniteCollection(col)
//    }
//
//  private[enumeration] def fromFiniteCollection[T](col: Array[T]): Finite[T] =
//    col match {
//      case _ if col.size == 0 => Empty
//      case _ if col.size == 1 => Singleton(col.head)
//      case _ => WrapArray(col)
//    }
//
//  private[enumeration] def fromFiniteCollection[T](col: Traversable[T])
//    (implicit ct: ClassTag[T]): Finite[T] =
//    col match {
//      case _ if col.size == 0 => Empty
//      case _ if col.size == 1 => Singleton(col.head)
//      case _ => WrapArray(col.toIndexedSeq)
//    }
//
//  private[enumeration] def fromCollectionNoTag[T](col: Traversable[T]): Enum[T] =
//    col match {
//      case (stream: Stream[T]) if !stream.hasDefiniteSize => new WrapStream(stream)
//      case _ if col.size == 0 => e.Empty with ResetIter[T] with Touchable[T]
//      case _ if col.size == 1 => Singleton(col.head)
//      case is: IndexedSeq[T] => new WrapIndexedSeq(is)
//    }

}

// this does not help much, still wins over streamToEnum
trait EnumLessPriority {

  implicit def elToEnum[T](el: T) =
    Singleton(el)

}
