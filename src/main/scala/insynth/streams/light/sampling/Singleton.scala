package insynth.streams
package light
package sampling

import scala.util.Random

trait Singleton[T] extends SamplableEnum[T] {
  
  this: light.Singleton[T] =>
  
  override def sample(sampler: Random) =
    this.el
  
}