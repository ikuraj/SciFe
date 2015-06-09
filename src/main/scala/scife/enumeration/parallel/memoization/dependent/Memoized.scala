package scife.enumeration
package parallel
package memoization
package dependent

import scife.enumeration.dependent.Depend
import scife.enumeration.memoization._

import java.util.concurrent.ConcurrentHashMap

import scife.util.HasLogger

trait Memoized[I, O] extends Depend[I, O] with Memoizable with HasLogger {

  private[enumeration] val memoizedMap = new ConcurrentHashMap[I, EnumType]()
  
  override abstract def getEnum(parameter: I) = {
//    println("memoizedMap.contains(parameter) " + memoizedMap.contains(parameter))
    //memoizedMap.getOrElseUpdate(parameter, super.getEnum(parameter))
    if (memoizedMap containsKey parameter) {
      info(s"parameter=$parameter is memoized")
      memoizedMap.get(parameter)
    }
    else {
      info(s"parameter=$parameter is not memoized")
      val res = super.getEnum(parameter)
      memoizedMap.put(parameter, res)
      res
    }
  }

  override def clearMemoization = memoizedMap.clear
  
  // helper (debugging) method
  protected[enumeration] def isMemoized(el: I) = memoizedMap contains el
  
}
