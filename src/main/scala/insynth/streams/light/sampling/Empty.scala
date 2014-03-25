package insynth.streams
package light
package sampling

import scala.util.Random

object Empty extends light.Empty with SamplableEnum[Nothing] {
  
  override def sample(sampler: Random) =
    throw new NoSuchElementException("no elements in Empty") 
  
}