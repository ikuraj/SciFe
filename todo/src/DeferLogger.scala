package insynth.util.logging

import scala.util.logging.Logged

trait DeferLogger extends Logged {
  
	import DeferLogger._
  
  override def log(msg: String): Unit = {
  	if (loggingCondition)
    super.log(msg)
  }
}

object DeferLogger {
  var loggingCondition = false  
}