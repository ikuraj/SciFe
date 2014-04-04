package insynth.enumeration
package reverse

case class Singleton[T](override val el: T) extends SingletonT[T] with Reverse[T] {
  
  override def reverse[V >: T](a: V) =
    if (a == el) 0
    else throw new IllegalArgumentException(("Given element %s is not equal to singleton " +
      "element %s").format(a.toString, el.toString))

}