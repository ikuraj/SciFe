package scife
package enumeration

import scife.util._

import scala.reflect._

class WrapIndexedSeq[@specialized T](val coll: IndexedSeq[T])
  extends Finite[T] with HasLogger {
  require(coll.hasDefiniteSize && coll.size > 1)

  override def size = coll.size

  override def apply(ind: Int) = coll(ind)

}
