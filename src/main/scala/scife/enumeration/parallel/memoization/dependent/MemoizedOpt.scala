package scife.enumeration
package parallel
package memoization
package dependent

import scife.enumeration.dependent.Depend
import scife.enumeration.memoization._

import java.util.concurrent.ConcurrentHashMap

import scife.util.HasLogger

import scala.language.higherKinds

trait MemoizedOpt[I, O] extends Depend[I, O] with Memoizable with HasLogger {
  import scife.enumeration.memoization.lzy._
  
//  type EnumSort[A] <: MemoizedStaticOpt[A]
  private[enumeration] val memoizedMap = new ConcurrentHashMap[I, EnumType]()
  
  override abstract def getEnum(parameter: I) = {
//    println("memoizedMap.contains(parameter) " + memoizedMap.contains(parameter))
    //memoizedMap.getOrElseUpdate(parameter, super.getEnum(parameter))
    if (memoizedMap containsKey parameter) {
//      println(s"parameter=$parameter is memoized")
      memoizedMap.get(parameter)
    }
    else {
//      println(s"parameter=$parameter is not memoized")
      var res = super.getEnum(parameter)
      val inSide = memoizedMap.containsKey(parameter)
      var existingVal = memoizedMap.putIfAbsent(parameter, res) 
      assert(existingVal != null || inSide != true)

      // a small hack
      if (existingVal != null) {
//        println(s"parameter=$parameter memoized, 2nd try" + (existingVal != null) + " " + (existingVal.isInstanceOf[MemoizedStaticOpt[O]]) + " " +
//          (res.isInstanceOf[MemoizedStaticOpt[O]]))
        if (existingVal.isInstanceOf[MemoizedStaticOpt[O]] &&      
          res.isInstanceOf[MemoizedStaticOpt[O]]) {
          println("optimizing")
          // put failed
          existingVal.asInstanceOf[MemoizedStaticOpt[O]].merge(res.asInstanceOf[MemoizedStaticOpt[O]])
        }
        res = existingVal
      }
//      memoizedMap.put(parameter, res)
      res
    }
  }

  override def clearMemoization = memoizedMap.clear
  
  // helper (debugging) method
  protected[enumeration] def isMemoized(el: I) = memoizedMap contains el
  
}
