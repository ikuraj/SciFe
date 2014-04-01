package insynth.enumeration
package reverse

case class Singleton[T](override val el: T) extends SingletonT[T] with Reverse[T] {
  
  override def reverse[V >: T](a: V) =
    if (a == el) this
    else throw new IllegalArgumentException

}