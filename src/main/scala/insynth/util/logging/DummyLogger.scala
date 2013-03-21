package insynth.util.logging

class DummyLogger {

  def severe(msg: => String) = {} 

  def warning(msg: => String) = {}

  def info(msg: => String) = {}

  def fine(msg: => String) = {}

  def finer(msg: => String) = {}

  def finest(msg: => String) = {}
  
}