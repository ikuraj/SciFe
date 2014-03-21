package insynth
package streams
package light

import scala.reflect._

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

class WrapperIndexedSeq[@specialized T](val coll: IndexedSeq[T])
	extends Finite[T] with HasLogger {
  require(coll.hasDefiniteSize)
  
  override def size = coll.size
  
  override def apply(ind: Int) = coll(ind)
    
}