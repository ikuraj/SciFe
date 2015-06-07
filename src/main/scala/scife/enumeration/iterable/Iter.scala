package scife.enumeration
package iterable

import scife.util.HasLogger

trait Iter[+A] extends Enum[A] with HasLogger {
  
//  self: Enum[A] =>
    
  protected var ind = -1
  
  def head = apply(ind)
  
  def hasNext = {
//    entering(s"hasNext at ${this.getClass.getSuperclass.getName}", ind, this.size)
    ind + 1 < this.size
  }
  
  def next = {
//    entering(s"next at ${this.getClass.getSuperclass.getName}", ind)
    ind += 1
    this.apply(ind)
  }
  
}