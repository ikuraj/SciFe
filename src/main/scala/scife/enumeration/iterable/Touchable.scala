package scife
package enumeration
package iterable

trait Touchable[+A] extends Iter[A] {
  
  self: Enum[A] =>
    
  var touched = false
    
  abstract override def next = {
    touched = true
    super.next
  }

}