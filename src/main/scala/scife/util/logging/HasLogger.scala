package scife.util.logging

/**
 * Classes can mix this trait for having access to the "default" {{{logger}}}.
 *
 * Clients can inject different loggers if needed.
 */
private[util] trait HasLogger {

  // ScalaLogging levels
  // Error // Warn // Info // Debug // Trace
  
  protected[this] def getMyClass = this.getClass

  @transient
  protected[this] lazy val logger =
    LoggerFactory.newLogger(getMyClass.getName)
    
  protected[this] def loggerFactory = LoggerFactory

  def severe(msg: => String) = logger.error(msg)

  def error(msg: => String) = logger.error(msg)

  def warning(msg: => String) = logger.warn(msg)

  def info(msg: => String) = logger.info(msg)

  def fine(msg: => String) = logger.debug(msg)

  def finer(msg: => String)  = logger.debug(msg)

  def finest(msg: => String) = logger.trace(msg)

  def entering(method: => String, arguments: Any*) =
    logger.trace("Entering " + getMyClass + "." + method + " with: " + arguments.mkString(", "))

  def exiting(method: => String, result: => String) =
    logger.trace("Exiting " + getMyClass + "." + method + " with " + result)

  def exiting[T](method: => String, result: T): T =
    { logger.trace("Exiting " + getMyClass + "." + method + " with " + result); result }
}
