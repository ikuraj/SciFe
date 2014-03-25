package insynth.streams
package light
package sampling

import insynth.util.logging._
import dependent.BinaryFiniteCombineLazy

import scala.util.Random

trait BinaryChain[I, O, R] extends Enum[R] with Samplable[R] with HasLogger {
  
  this: BinaryFiniteCombineLazy[I, O, R] with Dependent[I, R] =>
    
  def sample(sampler: Random) = {
    val firstLevelIndex = sampler.nextInt( this.s1.size )
    
    val innerEnum = this.getStream( s1(firstLevelIndex) )
    
    innerEnum.sample( sampler )
  }
  
}