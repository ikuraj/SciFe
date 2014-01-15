package insynth.streams

/**
 * OrderedStreamable is a Streamable that enumerates a (usually) cumulative weights with
 * associated enumerated elements
 * @param <T> Type of enumerated elements
 * @param <V> Values for determine "weights" of elements
 */
trait OrderedStreamable[+T, @specialized(Int, Float) V] extends Streamable[T] with Valuable[V] {
    
  override def isInfinite: Boolean
  
  final override def getStream = getValuedStream map { _._1 }
  
  def getValuedStream: Stream[(T, V)]
  
  final override def getValues = getValuedStream map { _._2 }
  
}