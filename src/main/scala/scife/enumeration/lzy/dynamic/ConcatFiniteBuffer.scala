package scife.enumeration
package lzy
package dynamic

import scala.collection.mutable

// Concatenation of finite enumerators, new enumerators can be added dynamically
class ConcatFiniteBuffer[T] protected[enumeration] (streams: Seq[Finite[T]])
  extends ConcatFinite[T] {

  private var streamsArray = mutable.ArrayBuffer(streams: _*)

  override def length = streamsArray.length

  var _size = 0
  private var limits =
    mutable.ArrayBuffer(0) ++= {
      for (stream <- streams) yield {
        _size += stream.size
        _size
      }
    }

  def append(en: Finite[T]) {
    streamsArray += en
    _size += en.size
    limits += _size
  }

  override def size = _size

  override def enum(ind: Int) = streamsArray(ind)

  override def limit(ind: Int) = limits(ind)

}
