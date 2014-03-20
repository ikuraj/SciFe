package insynth.streams
package light.weight

trait Weighted[@specialized(Int, Float) T] {
  def getWeight(ind: Int): T
}