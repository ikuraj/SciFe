package insynth.util

class TimeOut(timeSlot:Long) {

  private var startTime:Long = 0;
  
  def start(){
    startTime = System.currentTimeMillis
  }
  
  def hasExpired() = System.currentTimeMillis - startTime >= timeSlot
}

object TimeOut{
  def apply(timeSlot:Long) = new TimeOut(timeSlot)
}