package scife.util.logging

import scala.collection._

/** 
 * Extends HasLogger by counting how many times a particular method was invoked
 * on a particular object
 */
trait ProfileLogger extends HasLogger {
  import System.{ currentTimeMillis => _time }
  
  var countMap: mutable.Map[(Int, String), (Long, Int)] =
    mutable.Map[(Int, String), (Long, Int)]().withDefaultValue((0l, 0))

  def profile[R](code: => R) = {
    val startTime = _time
    val res = code
    info( "Profiling took " + (_time - startTime) + "ms")
    res
  }

  def profile[R](name: String)(code: => R) = {
    val startTime = _time
    val res = code
    info( name + " took " + (_time - startTime) + "ms")
    res
  }
  
  private def getTime[R](code: => R) = {
    val startTime = _time
    val res = code
    _time - startTime
  }
  
  def averageProfile[R](code: => R, times: Int, name: => String = "Profiling") = {
    for (_ <- 1 to times) {
      val (accTime, numExecutions) = countMap((this.##, name))
      val time = getTime(code)
      countMap((this.##, name)) = ( (accTime + time), (numExecutions + 1) )
      info( name + " took " + ((accTime + time)/(numExecutions + 1)) + "ms (average from " + (numExecutions + 1) + " runs.")
    }
    code
  }
     
}