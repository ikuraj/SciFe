package scife.enumeration
package memoization

import scala.collection.mutable._

import scife.util._

trait MemoizedSize extends Enum[Any] {

  override abstract val size = super.size

}
