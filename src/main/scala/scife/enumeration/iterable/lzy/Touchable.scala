package scife
package enumeration
package iterable
package lzy

trait Touchable[+A] extends Iter[A] {
  
  self: Enum[A] =>
    
  var touched = false
    
  abstract override def next = {
    touched = true
    super.next
  }

}