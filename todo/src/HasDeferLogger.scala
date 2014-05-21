package insynth.util.logging

import scala.util.logging.{ Logged, ConsoleLogger }

/** 
 * Defer logging for flexibility
 */
trait HasDeferLogger extends HasLogger {
  
	import DeferLogger._
    		
  def startLogging = {
	  loggingCondition = true
	}
    
  def stopLogging(flush: Boolean = false) = {
	  loggingCondition = false
	}

}