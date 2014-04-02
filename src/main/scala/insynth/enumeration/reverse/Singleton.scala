package insynth.enumeration
package reverse

case class Singleton[I, T](override val el: T) extends SingletonT[T] with Reverse[I, T] {
  
  override def reverse[V >: T](a: V) =
    if (a == el) 0
    else throw new IllegalArgumentException

}