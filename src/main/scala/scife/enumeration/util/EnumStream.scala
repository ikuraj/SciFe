package scife.enumeration
package util

// EnumStream is by assumption infinite
object EnumStream {

  def apply[A](ind: Int, enum: Enum[A]): Stream[A] = {
    new EnumStream(ind, enum)
  }

  def apply[A](enum: Enum[A]): Stream[A] = {
    require(enum.hasDefiniteSize == false)
    this(0, enum)
  }

}

/** Same as Scala stream but strictly single-threaded, thus without synchronization */
class EnumStream[+A](ind: Int, enum: => Enum[A]) extends Stream[A] {

  /* always built from an infinite Enum */
  override def isEmpty = false
  override def hasDefiniteSize = false

  override def head = enum(ind)
  override def tail = {
    if (!tailDefined) tlVal = EnumStream(ind + 1, enum)

    tlVal
  }

  private[this] var tlVal: Stream[A] = _
  def tailDefined: Boolean = tlVal ne null

}