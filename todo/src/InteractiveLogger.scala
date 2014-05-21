package insynth.util.logging

import scala.util.logging._

/** 
 * Pauses after logging
 */
trait InteractiveLogger extends Logged {  
  
  def interactivePause = {
    System.out.println("Press Any Key To Continue...");
    new java.util.Scanner(System.in).nextLine();
  }
  
  override def log(msg: String): Unit = {
    super.log(msg)
    interactivePause
  }
  
  def wrapper(msg: => Unit) {
    msg
    interactivePause
  }
}