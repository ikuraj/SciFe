package scife
package enumeration

import scala.language.implicitConversions

package object memoization {
  
  type Memoized[T] = MemoizedStatic[T] with MemoizedSize

  import scope._
  
  // convenient way of adding enumerators to implicit memoization scope
  class ScopeHelper[A <: Memoizable](m: A) {
    def unary_!(implicit ms: MemoizationScope) = {
      ms add m
      m
    }
  }
  
  implicit def toScopeHelper[A <: Memoizable](m: A) = new ScopeHelper(m)
  
  def ![T](m: Memoized[T])(implicit ms: MemoizationScope) = {
    ms add m
    m
  }

  def addToScope[T](m: Memoized[T])(implicit ms: MemoizationScope) = {
    ms add m
    m
  }
  
}