package insynth.streams
package dependent

trait DependentStreamable[I, +O] {

  def getStream(parameter: I): Stream[O]
  
}