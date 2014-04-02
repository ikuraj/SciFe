package insynth.enumeration
package reverse

import scala.reflect._

object Reverser {

  def apply[I, T](arg: T, args: T*)(implicit ct: ClassTag[T]): Reverse[I, T] =
    fromFiniteCollection(arg :: args.toList)

  private[enumeration] def fromFiniteCollection[I, T](col: Traversable[T])
    (implicit ct: ClassTag[T]): Reverse[I, T] =
    col match {
      case _ if col.size == 0 => Empty
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