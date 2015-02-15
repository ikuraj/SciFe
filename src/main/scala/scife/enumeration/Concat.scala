package scife.enumeration

import scala.reflect._

object Concat {

  def apply[T](left: Enum[T], right: Enum[T]): Enum[T] =
    lzy.Concat(left, right)

  def apply[T](left: Finite[T], right: Finite[T]): Finite[T] =
    lzy.Concat(left, right)
    
  def apply[T](coll: Traversable[Finite[T]]): Enum[T] =
    coll match {
      case s: Stream[Finite[T]] => throw new RuntimeException
      case _ => this(coll.toArray)
    }

  def apply[T, U <: Enum[T]](streams: Array[U])(implicit ct: ClassTag[U]): Enum[T] =
    lzy.Concat[T, U](streams)(ct)
    
  def apply[T](arg1: Finite[T], args: Finite[T]*): Finite[T] =
    this( (arg1 :: args.toList).toArray )

  def apply[T](finites: Array[Finite[T]]): Finite[T] =
    lzy.Concat(finites)

  def apply[T](infinites: Array[Infinite[T]]): Enum[T] =
    lzy.Concat(infinites)

}
