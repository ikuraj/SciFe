package scife.util.logging

import scala.collection._

import scala.language.postfixOps

/** 
 * Extends HasLogger by counting how many times a particular method was invoked
 * on a particular object
 */
trait CountedLogger extends HasLogger {
  
  var countMap: mutable.Map[(Int, String), Int] =
    mutable.Map[(Int, String), Int]().withDefaultValue(0)
    
  override def entering(method: => String, arguments: Any*) = {    
    lazy val newCount = countMap((this.##, method)) + 1
    countMap((this.##, method)) += 1
	  super.entering(method + "(calls:" + newCount + ")", arguments)
  }

  var counterAcrossInstancesMap: mutable.Map[(Class[_], Int, String), Int] =
    mutable.Map[(Class[_], Int, String), Int]()
    
  def addValue(method: => String, v: Int) = {  
    counterAcrossInstancesMap((getMyClass, this.##, method)) = v
    val sum =
      (for ( ((clazz, hash, meth), mapVal) <- counterAcrossInstancesMap; if clazz == getMyClass && meth == method)
        yield mapVal) sum
        
    fine( "Cummulative value for class %s, is %d: ".format(getMyClass.getSimpleName, sum) )    
  }
     
}