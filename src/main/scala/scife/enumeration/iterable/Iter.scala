package scife.enumeration
package iterable

trait Iter[+A] extends Enum[A] {
  
//  self: Enum[A] =>
    
  protected var ind = -1
  
  def head = apply(ind)
  
  def hasNext = ind + 1 == this.size
  
  def next = {
    ind += 1
    this.apply(ind)
  }
  
}