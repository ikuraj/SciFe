package insynth.streams

/**
 * Each streamable may have elements to be enumerated but they may not
 * be ready for enumeration at a given point (think of situations in which
 * recursive links need to be followed) 
 * @param <T> Type of enumerated elements
 */
trait OrderedStreamable[+T] extends Streamable[T] with Valuable[Int] {
    
  /**
   * @return Whether this streamable has more elements to enumerate 
   */
  def isDepleted: Boolean
  /**
   * Check if the element with the index i is ready to be enumerated
   */
  def nextReady(i: Int): Boolean
  
  def isInfinite: Boolean
  
  def getStream: Stream[T]
  
}