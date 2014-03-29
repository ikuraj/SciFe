package insynth.enumeration

import scala.reflect._

object Concat {
  
  def apply[T](left: Enum[T], right: Enum[T]) =
    lzy.Concat(left, right)
  
  def apply[T, U <: Enum[T]](streams: Seq[U])(implicit ct: ClassTag[U]) =
    lzy.Concat[T, U](streams)(ct)
  
  def apply[T](finites: Array[Finite[T]]) =
    lzy.Concat(finites)
  
  def apply[T](infinites: Seq[Infinite[T]]) =
    lzy.Concat(infinites)
  
}