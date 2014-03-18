package insynth.streams.light

/** Same as Scala stream but strictly single-threaded, thus without synchronization */
class EnumStream[+A](ind: Int, enum: Enum[A]) extends Stream[A] {

  override def isEmpty = false
  override def hasDefiniteSize = false
  override def head = enum(ind)
  @volatile private[this] var tlVal: Stream[A] = _
  def tailDefined: Boolean = tlVal ne null
  override def tail = {
    if (!tailDefined)
      synchronized {
        if (!tailDefined) tlVal = EnumStream(ind + 1, enum)
      }

    tlVal
  } 
    
}

object EnumStream {
  
  def apply[A](ind: Int, enum: => Enum[A]): Stream[A] = {
    if (ind < enum.size || enum.size < 0)
      new EnumStream(ind, enum)
    else Stream.Empty
  }
  
  def apply[A](enum: => Enum[A]): Stream[A] = {
    this(0, enum)
  }
  
}