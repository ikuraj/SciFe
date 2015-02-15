package scife.enumeration
package member

import _root_.scife.{ enumeration => e }

class Empty[T] extends e.Empty with MemberFinite[T] {
  
  override def member(a: T) = false
  
}