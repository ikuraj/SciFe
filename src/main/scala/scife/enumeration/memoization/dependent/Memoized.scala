package scife.enumeration
package memoization
package dependent

import scife.enumeration.dependent.Depend

import scala.collection.mutable

trait Memoized[I, O] extends Depend[I, O] with Memoizable {

  private[enumeration] val memoizedMap = mutable.Map[I, EnumType]()
  
  override abstract def getEnum(parameter: I) = {
//    println(s"memoizedMap.contains($parameter) " + memoizedMap.contains(parameter))
    //memoizedMap.getOrElseUpdate(parameter, super.getEnum(parameter))
    if (memoizedMap contains parameter) {
      println(s"memoizedMap getting ${memoizedMap(parameter)}")
      memoizedMap(parameter)
    }
    else {
      val res = super.getEnum(parameter)
      memoizedMap(parameter) = res
      res
    }
  }

  override def clearMemoization = memoizedMap.clear
  
  // helper (debugging) method
  protected[enumeration] def isMemoized(el: I) = memoizedMap contains el
  
}
