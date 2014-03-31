package insynth.enumeration
package memoization
package dependent

import insynth.enumeration.dependent.Depend

import scala.collection.mutable

trait Memoized[I, O] extends Depend[I, O] with Memoizable {

  val memoizedMap = mutable.Map[I, EnumType]()
  
  override abstract def getEnum(parameter: I) = {
    memoizedMap.getOrElseUpdate(parameter, super.getEnum(parameter))
  }
  
  override def clearMemoization = memoizedMap.clear
  
}