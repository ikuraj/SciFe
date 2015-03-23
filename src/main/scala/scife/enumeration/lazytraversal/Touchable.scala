package scife
package enumeration
package lazytraversal

import scife.util._

trait Touchable[+A] extends Enum[A] with HasLogger {
  
//  self: Enum[A] =>
    
//  @volatile
  private var touched = true
    
  abstract override def apply(ind: Int): A = {
//    println(s"apply[$ind]$hashCode")
    if (this.isInstanceOf[scife.enumeration.WrapArray[_]]) println(s"apply from $hashCode")
    touched = true
    val res = super.apply(ind)
    if (this.isInstanceOf[scife.enumeration.WrapArray[_]]) println(s"end apply from $hashCode")
    res
  }
  
  def reset = touched = false

}