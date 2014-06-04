package insynth.enumeration
package member

case class Singleton[T](override val el: T) extends SingletonT[T] with MemberFinite[T] {
  
  override def member(a: T) =
    a == el

}