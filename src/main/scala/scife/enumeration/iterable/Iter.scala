package scife.enumeration
package iterable

trait Iter[+A] /*extends Iterator[A]*/ {
  
  self: Enum[A] =>
    
  protected var ind = -1
  
  def head = self(ind)
  
  def hasNext = ind + 1 == self.size
  
  def next = {
    ind += 1
    self(ind)
  }
  
}