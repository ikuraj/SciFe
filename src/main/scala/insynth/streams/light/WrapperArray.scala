package insynth
package streams
package light

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

class WrapperArray[T](coll: IndexedSeq[T])
	extends Finite[T] with HasLogger {
  require(coll.hasDefiniteSize)
  
  override def size = coll.size
  
  override def apply(ind: Int) = coll(ind)
    
}

object WrapperArray{

  def apply[T](stream: IndexedSeq[T]) =
  	new WrapperArray(stream)

  def apply[T](args: T*) =
  	new WrapperArray(IndexedSeq(args: _*))

}