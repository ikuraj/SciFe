package insynth.enumeration
package eager

import combinators._

import Filter.FilterFunction

abstract class SimpleFilter[T](val enum: Enum[T], override val f: FilterFunction[T])
  extends Filter[T] {
  
  override def apply(ind: Int) = {
    var corrInd=0
    var i = 0
    import scala.util.control.Breaks._

    breakable {
      while (true) {
        if ( f (enum(i)) ) corrInd+=1
        if ( corrInd >= ind) break
        i+=1
      }
    }
    
    enum(i)
  }
  
  override def size = {
    var i=0
    while (i < enum.size) {
      if (f (enum(i)) ) i+=1
    }
    
    i
  }
  
}