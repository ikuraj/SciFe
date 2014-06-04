package insynth.enumeration
package member

import _root_.insynth.{ enumeration => e }

class Empty[T] extends e.Empty with MemberFinite[T] {
  
  override def member(a: T) = false
  
}