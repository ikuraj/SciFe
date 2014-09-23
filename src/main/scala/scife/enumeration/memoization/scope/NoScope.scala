package scife.enumeration
package memoization
package scope

object NoScope extends MemoizationScope {
  
  def add(m: Memoizable) = { }

  def clear = { }

}