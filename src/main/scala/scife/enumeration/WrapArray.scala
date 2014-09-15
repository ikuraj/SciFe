package scife
package enumeration

import scala.reflect._

class WrapArray[@specialized T](val coll: Array[T])
	extends Finite[T]
  with Serializable {
//  require(coll.hasDefiniteSize && coll.size > 1)
  // check size in the factory that constructs this
  require(coll.hasDefiniteSize)
  
  override def size = coll.size
  
  override def apply(ind: Int) = coll(ind)
    
}

object WrapArray {

  def apply[@specialized T](stream: IndexedSeq[T])(implicit ct: ClassTag[T]) =
  	new WrapArray(stream.toArray)

  def apply[@specialized T](array: Array[T]) =
    new WrapArray(array)

  def apply[@specialized T](args: T*)(implicit ct: ClassTag[T]) =
  	new WrapArray(Array(args: _*))

}