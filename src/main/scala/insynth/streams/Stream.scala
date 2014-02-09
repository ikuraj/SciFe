package insynth.streams

/** Same as Scala stream but strictly single-threaded, thus without synchronization */
class Cons[+A](hd: A, tl: => Stream[A]) extends Stream[A] with Serializable {

  override def isEmpty = false
  override def head = hd

  private[this] var tlVal: Stream[A] = _

  def tailDefined: Boolean = tlVal ne null
  override def tail: Stream[A] = {
    tlVal = tl
    tlVal
  }
}