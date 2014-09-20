package scife
package enumeration
package reverse

import scife.{ enumeration => e }

import scala.{ collection => col }
import scala.reflect._

class WrapArray[T](override val coll: Array[T]) extends e.WrapArray[T](coll)
	with ReverseFinite[T] with HasLogger {
  
  val reverseMap: col.Map[T, Int] = coll.zipWithIndex.toMap
  info("reverseMap is " + reverseMap)
  
  override def reverse(el: T) = {
    val revInd = reverseMap(el)
    revInd
//    new WrapArray(coll) with Reversed[T] {
//      override val pos = revInd
//    }
  }

}
