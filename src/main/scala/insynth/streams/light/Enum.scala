package insynth.streams.light

import scala.reflect._
import scala.language.implicitConversions

object Enum {
  
  implicit def colToEnum[T](col: Seq[T])(implicit ct: ClassTag[T]) =
    apply(col)
    
  implicit def elemsToEnum[T](elems: T*)(implicit ct: ClassTag[T]) =
    apply(elems)

  implicit def streamToEnum[T](col: Stream[T]): Enum[T] =
    applyNoTag(col)

  implicit def traversableToEnum[T](col: Traversable[T]): Enum[T] =
    applyNoTag(col)
    
  implicit def enumToCollection[T](e: Enum[T]): Seq[T] =
    if (e.hasDefiniteSize)
      e.toList
    else EnumStream(e)

  def apply[T](col: Traversable[T])(implicit ct: ClassTag[T]): Enum[T] =
    col match {
      case (stream: Stream[T]) if !stream.hasDefiniteSize => new WrapperStream(stream)
    	case _ if col.size == 0 => Empty
    	case _ if col.size == 1 => Singleton(col.head)
    	case _ => WrapperArray(col.toIndexedSeq)
	  }
  
  def applyNoTag[T](col: Traversable[T]): Enum[T] =
    col match {
      case (stream: Stream[T]) if !stream.hasDefiniteSize => new WrapperStream(stream)
      case _ if col.size == 0 => Empty
      case _ if col.size == 1 => Singleton(col.head)
      case is: IndexedSeq[T] => new WrapperIndexedSeq(is)
    }

  def applyNoTag[T](col: Array[T]): Enum[T] =
    col match {
      case _ if col.size == 0 => Empty
      case _ if col.size == 1 => Singleton(col.head)
      case _ => WrapperArray(col)
    }

//  def apply[T](col: Stream[T])(implicit ct: ClassTag[T]): Enum[T] =
//    col match {
//    	case _ if col.size == 0 => Empty
//    	case _ if col.size == 1 => Singleton(col.head)
//    	case (stream: Stream[T]) if stream.hasDefiniteSize => new WrapperStream(stream)
//    	case _ => WrapperArray(col.toIndexedSeq)
//	  }

//  def apply[T](args: T*)(implicit ct: ClassTag[T]): Enum[T] =
//    args match {
//    	case _ if args.size == 0 => Empty
//    	case _ if args.size == 1 => Singleton(args.head)
//    	case _ => WrapperArray(args.toIndexedSeq)
//  	}

  implicit def rangeToEnum(range: Range): Enum[Int] =
    WrapperArray(range)
  
}

trait Enum[+A] {
  
  def size: Int
  
  def hasDefiniteSize: Boolean
  
  def apply(ind: Int): A
  
  def toList = ( for (i <- 0 until size) yield this(i) ).toList
  
  // operators
  def concat[B](e: Enum[B]) =
    RoundRobbin(this, e)
    
  def ++[B](e: Enum[B]) = concat(e)
  def ⊕[B](e: Enum[B]) = concat(e)

  def product[B](e: Enum[B]): Enum[(A, B)] =
    Binary(this, e)
    
  def **[B](e: Enum[B]) = product(e)
  def ⊗[B](e: Enum[B]) = product(e)

  def map[B](modifyFun: A => B): Enum[B] =
    Mapper(this, modifyFun)
    
  def ↑[B](modifyFun: A => B) = map(modifyFun)

//  def filter[B](e: Enum[B]) =
//    FilterStream(this, e)
//    
//  def **[B](e: Enum[B]) = product(e)
//  def ⊘[B](e: Enum[B]) = product(e)
  
}

trait Finite[+A] extends Enum[A] {
  
  def hasDefiniteSize = true
  
//  self: Enum[A] =>
  
//  lazy val memoizedSize: Int = self.size
//  
//  override def size = memoizedSize
  
}

trait Infinite[+A] extends Enum[A] {
  
  def hasDefiniteSize = false
  
  override def size = -1
  
}