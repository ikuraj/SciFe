package insynth.util.logging

trait DummyLogger {

  def severe(msg: => String) = {} 

  def error(msg: => String) = {}

  def warning(msg: => String) = {}

  def info(msg: => String) = {}

  def fine(msg: => String) = {}

  def finer(msg: => String) = {}

  def finest(msg: => String) = {}
  
  def entering(method: => String, arguments: Any*) = {}
    
  def exiting(method: => String, result: => String) = {}
   
  def exiting[T](method: => String, result: T): T = result
  
}