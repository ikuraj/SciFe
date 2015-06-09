package scife.enumeration
package iterable

trait ResetIter[+A] extends Iter[A] {
  
  _: Enum[A] =>
  
  def hasStarted = ind >= -1
  
  def reset = ind = -1
    
}

//trait ResetIterFinite[+A] extends Finite[A] with ResetIter[A]