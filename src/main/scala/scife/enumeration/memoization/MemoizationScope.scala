package scife.enumeration
package memoization

trait MemoizationScope extends Serializable {

  def add(m: Memoizable)

  def clear

}

object MemoizationScope {
  
  // default to no scope if not implicit scope is found
  implicit val noScopeObject = scope.NoScope
  
}