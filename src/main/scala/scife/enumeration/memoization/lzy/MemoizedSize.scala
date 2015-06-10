package scife.enumeration
package memoization
package lzy

import scala.collection.mutable._

import scife.util._

trait MemoizedSize extends Enum[Any] {

  override abstract def size = {
    if (_size == -1)
      _size = super.size

    _size
  }
  
  var _size: Int = -1
}