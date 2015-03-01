package scife
package enumeration
package iterable
package lzy

import scife.util._

trait Touchable[+A] extends Enum[A] with Iter[A] with HasLogger {
  
//  self: Enum[A] =>
    
  var touched = true
    
  abstract override def next = {
    entering("next[Touchable]", ind)
    touched = true
    super.next
  }
  
//  abstract override def apply(ind: Int) = {
//    entering("apply[Touchable]", ind)
//    touched = true
//    super.apply(ind)
//  }

}