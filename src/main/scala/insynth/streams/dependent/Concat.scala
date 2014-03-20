package insynth.streams
package dependent

import scala.collection.mutable.{ ArrayBuffer, MutableList }

import insynth.streams.light.Finite
import insynth.util.logging._

import scala.language.postfixOps
import scala.reflect._
import scala.annotation._

class Concat[I, O]
  (s1: Dependent[I, O], s2: Dependent[I, O])
  extends Dependent[I, O] with HasLogger {
  
  def getStream(parameter: I) = {
    val left = s1.getStream(parameter)
    val right = s2.getStream(parameter)
    
    light.RoundRobbin(left, right)    
  }
  
}