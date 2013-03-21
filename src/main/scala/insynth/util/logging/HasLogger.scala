package insynth.util.logging

/** 
 * Classes can mix this trait for having access to the "default" {{{logger}}}.
 *  
 * Clients can inject different loggers if needed.
 */
trait HasLogger {  
  
  protected[this] def getMyClass = this.getClass
  
  protected[this] lazy val logger =
	LoggerFactory.newLogger(getMyClass.getName)      		   
    
  def severe(msg: => String) = logger.fatal(msg)

  def error(msg: => String) = logger.error(msg)
  
  def warning(msg: => String) = logger.warn(msg)  
     
  def info(msg: => String) = logger.info(msg)
   
  def fine(msg: => String) = logger.debug(msg)
   
  def finer(msg: => String)  = logger.debug(msg)
   
  def finest(msg: => String) = logger.trace(msg)
     
  def entering(method: => String, arguments: Any*) =
	  logger.trace("Entering " + getMyClass + "." + method)
     
  def exiting(method: => String, result: => String) =
	  logger.trace("Exiting " + getMyClass + "." + method + " with " + result)
}