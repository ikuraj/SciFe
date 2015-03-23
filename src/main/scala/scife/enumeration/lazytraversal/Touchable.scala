package scife
package enumeration
package lazytraversal

import scife.util._

trait Touchable[+A] extends Enum[A] with HasLogger {
  
  self: Resetable[A] =>
    
//  @volatile
    
  abstract override def apply(ind: Int): A = {
//    println(s"apply[$ind]$hashCode")
//    if (this.isInstanceOf[scife.enumeration.WrapArray[_]]) println(s"apply from $hashCode")
//    println(s"apply from $hashCode")
    _touched = true
    val res = super.apply(ind)
//    if (this.isInstanceOf[scife.enumeration.WrapArray[_]]) println(s"end apply from $hashCode")
    res
  }
  
//  def reset = _touched = false

}

trait Resetable[+A] {
  
  protected var _touched = true
  def touched = _touched
    
  def reset = _touched = false
}