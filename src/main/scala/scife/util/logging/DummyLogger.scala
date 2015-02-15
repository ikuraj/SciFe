package scife.util.logging

import scala.annotation._, elidable._

private[util] trait DummyLogger extends HasLogger {

  @elidable(SEVERE)
  override def severe(msg: => String) = {}

  @elidable(WARNING)
  override def error(msg: => String) = {}

  @elidable(WARNING)
  override def warning(msg: => String) = {}

  @elidable(INFO)
  override def info(msg: => String) = {}

  @elidable(FINE)
  override def fine(msg: => String) = {}

  @elidable(FINER)
  override def finer(msg: => String) = {}

  @elidable(FINEST)
  override def finest(msg: => String) = {}

  @elidable(FINEST)
  override def entering(method: => String, arguments: Any*) = {}

  @elidable(FINEST)
  override def exiting(method: => String, result: => String) = {}

  @elidable(FINEST)
  override def exiting[T](method: => String, result: T): T = result

}
