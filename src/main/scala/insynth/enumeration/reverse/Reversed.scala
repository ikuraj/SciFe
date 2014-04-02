package insynth.enumeration
package reverse

trait Reversed[T] extends Finite[T] {

  val pos: Int
  
  override def size = super.size - pos
  
  override def apply(ind: Int) = {
  	super.apply(ind + pos)
  }

}