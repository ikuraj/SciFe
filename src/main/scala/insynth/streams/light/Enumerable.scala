package insynth.streams.light

trait Enumerable[+A] {
  
  def size: Int
  
  def apply(ind: Int): A
  
}

trait Finite[+A] extends Enumerable[A] {
  
  self: Enumerable[A] =>
  
//  lazy val memoizedSize: Int = self.size
//  
//  override def size = memoizedSize
  
}

trait Infinite[+A] extends Enumerable[A] {
  
  override def size = -1
  
}

///** Same as Scala stream but strictly single-threaded, thus without synchronization */
//class Cons[+A](hd: A, tl: => Enumerable[A]) extends Enumerable[A] {
//
//  override def isEmpty = false
//  override def head = hd
//  override def tail: Enumerable[A] = tl
//  
//}
//
//object Enumerable {
//  
//  /** A wrapper class that adds `#::` for cons and `#:::` for concat as operations
//   *  to streams.
//   */
//  class ConsWrapper[A](tl: => Enumerable[A]) {
//    def #::(hd: A): Enumerable[A] = new Cons(hd, tl)
//    def #:::(prefix: Enumerable[A]): Enumerable[A] = prefix append tl
//  }
//
//  /** A wrapper method that adds `#::` for cons and `#::: for concat as operations
//   *  to streams.
//   */
//  implicit def consWrapper[A](stream: => Enumerable[A]): ConsWrapper[A] =
//    new ConsWrapper[A](stream)
//  
//}