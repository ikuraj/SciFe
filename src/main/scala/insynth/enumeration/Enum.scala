package insynth.enumeration

import util.EnumStream

import scala.reflect._
import scala.language.implicitConversions

trait Enum[+A] {
  
  def size: Int
  
  def hasDefiniteSize: Boolean
  
  def apply(ind: Int): A
  
  def toList = ( for (i <- 0 until size) yield this(i) ).toList
  
  /* operators */
  
  // concatenation
  def concat[B](e: Enum[B]) =
    Concat(this, e)
    
  def ++[B](e: Enum[B]) = concat(e)
  def ⊕[B](e: Enum[B]) = concat(e)

  // products
  def product[B](e: Enum[B]): Enum[(A, B)] =
    Product(this, e)
    
  def **[B](e: Enum[B]) = product(e)
  def ⊗[B](e: Enum[B]) = product(e)

  // map
  def map[B](modifyFun: A => B): Enum[B] =
    Map(this, modifyFun)
    
  def ↑[B](modifyFun: A => B) = map(modifyFun)
//
//  // filter
//  def filter[B](e: Enum[B]) =
//    throw new UnsupportedOperationException("Implement me!")
//    
//  def ⊘[B](e: Enum[B]) = filter(e)
  
}

object Enum {

  /* Factory methods */
  
  // only sequences are accepted since enumerators enumerate in a defined ordering
  def apply[T](arg: T, args: T*)(implicit ct: ClassTag[T]): Enum[T] =
    fromFiniteCollection(arg :: args.toList)

  def apply[T](stream: Seq[T])(implicit ct: ClassTag[T]): Enum[T] =
    fromCollection(stream)

  def apply(range: scala.Range) =
    if (range.start == 0) new IdentitySize(range.size)
    else new WrapRange(range)
    
  def identity = Identity

  def applyNoTag[T](args: T*): Enum[T] =
    fromCollectionNoTag(args)

  /* Implicit conversion methods */
  
  implicit def rangeToEnum(range: Range): Enum[Int] =
    apply(range)
  
  implicit def colToEnum[T](col: Seq[T])(implicit ct: ClassTag[T]) =
    fromCollection(col)
    
  implicit def elemsToEnum[T](elems: T*)(implicit ct: ClassTag[T]) =
    fromFiniteCollection(elems)

  implicit def streamToEnum[T](col: Stream[T]): Enum[T] =
    fromCollectionNoTag(col)

  implicit def traversableToEnum[T](col: Traversable[T]): Enum[T] =
    fromCollectionNoTag(col)
    
  implicit def enumToCollection[T](e: Enum[T]): Seq[T] =
    if (e.hasDefiniteSize)
      e.toList
    else EnumStream(e)

  private[enumeration] def fromCollection[T](col: Traversable[T])(implicit ct: ClassTag[T]): Enum[T] =
    col match {
      case (stream: Stream[T]) if !stream.hasDefiniteSize => new WrapStream(stream)
    	case _ => fromFiniteCollection(col)
	  }

  private[enumeration] def fromFiniteCollection[T](col: Traversable[T])(implicit ct: ClassTag[T]): Enum[T] =
    col match {
      case _ if col.size == 0 => Empty
      case _ if col.size == 1 => Singleton(col.head)
      case _ => WrapArray(col.toIndexedSeq)
    }
  
  private[enumeration] def fromCollectionNoTag[T](col: Traversable[T]): Enum[T] =
    col match {
      case (stream: Stream[T]) if !stream.hasDefiniteSize => new WrapStream(stream)
      case _ if col.size == 0 => Empty
      case _ if col.size == 1 => Singleton(col.head)
      case is: IndexedSeq[T] => new WrapIndexedSeq(is)
    }

}