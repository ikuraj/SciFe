package scife.enumeration
package member

import scala.reflect._
import scala.language.implicitConversions

trait Member[A] extends Enum[A] {
  
  def member(a: A): Boolean

//  def map[B >: A](modifyFun: A => B, recognize: B => A): Member[B] =
//    new Map(this, modifyFun, recognize) with Member[B] 

}

trait MemberFinite[A] extends Finite[A] with Member[A] {
 
  def map[B](modifyFun: A => B, recognize: B => A): MemberFinite[B] =
    new Map(this, modifyFun, recognize) with MemberFinite[B]
  
  def ⊗[B](e: MemberFinite[B]) =
    new ProductFinite(this, e)

}

trait MemberInfinite[A] extends Infinite[A] with Member[A]

object Member {

  def apply[T](arg: T, args: T*)(implicit ct: ClassTag[T]): Member[T] =
    fromFiniteCollection(arg :: args.toList)

  def apply[T](col: Traversable[T])(implicit ct: ClassTag[T]): Member[T] =
    fromFiniteCollection(col)

  private[enumeration] def fromFiniteCollection[I, T](col: Traversable[T])
    (implicit ct: ClassTag[T]): Member[T] =
    col match {
      case _ if col.size == 0 => new Empty
      case _ if col.size == 1 => new Singleton(col.head)
      case _ => new WrapArray(col.toArray)
    }
  
  def apply(range: Range) =
//    if (range.start == 0) new IdentitySize(range.size)
//    else
      new WrapRange(range)

}