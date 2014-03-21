package insynth
package streams
package light

import scala.reflect._

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

class WrapperArray[@specialized T](val coll: Array[T])
	extends Finite[T] with HasLogger {
  require(coll.hasDefiniteSize)
  
  override def size = coll.size
  
  override def apply(ind: Int) = coll(ind)
    
}

object WrapperArray{

  def apply[@specialized T](stream: IndexedSeq[T])(implicit ct: ClassTag[T]) =
  	new WrapperArray(stream.toArray)

  def apply[@specialized T](array: Array[T]) =
    new WrapperArray(array)

  def apply[@specialized T](args: T*)(implicit ct: ClassTag[T]) =
  	new WrapperArray(Array(args: _*))

}