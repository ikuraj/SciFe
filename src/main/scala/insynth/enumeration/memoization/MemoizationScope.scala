package insynth.enumeration
package memoization

import scala.collection.mutable.MutableList

class MemoizationScope extends Serializable {
  
  val memoizations = MutableList[Memoizable]()
  
  def add(m: Memoizable) = m +=: memoizations 
  
  def clear = for (m <- memoizations) m.clearMemoization

}