package scife
package enumeration
package member

import scife.{ enumeration => e }

import scala.{ collection => col }
import scala.reflect._

class WrapArray[T](override val coll: Array[T]) extends e.WrapArray[T](coll)
	with MemberFinite[T] with HasLogger {
  
  val reverseSet = coll.toSet
  
  override def member(el: T) = {
    reverseSet contains el
  }
    
}