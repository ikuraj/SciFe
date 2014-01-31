package insynth.streams

import insynth.util.logging.HasLogger

trait Counted[T] extends Streamable[T] {
  
  def enumerated: Int
  
}

package ordered {

  trait OrderedCountable[T] extends ordered.IntegerWeightStreamable[T] with Counted[T] with HasLogger {
    
    def enumerated: Int
    
  }
  
  trait OrderedCounted[T] extends ordered.IntegerWeightStreamable[T] with OrderedCountable[T] with HasLogger {
    
    var _enumerated = 0
    
    def enumerated = _enumerated
    
    override abstract def getValuedStream = {
      // each call to getValuedStream will remember its index of next element
      var nextIndex = 0
      val innerIterator = super.getValuedStream.iterator
      
      def loop: Stream[(T, Int)] = {
        if (innerIterator.hasNext) {
          nextIndex += 1
          if (nextIndex > _enumerated) _enumerated = nextIndex          
          innerIterator.next #:: loop
        } else Stream.empty
      }
      
      loop
    }
  }
}


//object Counted {
//  
//  def apply[T](streamable: Streamable[T]) = streamable match {
//    case os: ordered.Singleton[_] => new ordered.Singleton(os.element, os.value) with Counted[T]
//    case ss: ordered.WrapperStream[_] => new ordered.WrapperStream(ss.stream) with Counted[T]
//    case fs: ordered.FiniteStream[_] => new ordered.FiniteStream(fs.coll) with Counted[T]
//    case us: ordered.UnaryStream[_, _] => new ordered.UnaryStream(us.streamable) with Counted[T]
//    case obs: ordered.BinaryStream[_, _, _] => new ordered.BinaryStream(os.element, os.value) with Counted[T]
//    case ordered.Empty => new ordered.Empty(os.element, os.value) with Counted[T]
//    case lrr: ordered.LazyRoundRobbin[_] => new ordered.LazyRoundRobbin(os.element, os.value) with Counted[T]
//    case rr: ordered.RoundRobbin[_] => new ordered.RoundRobbin(os.element, os.value) with Counted[T]
//    case fs: ordered.FilterStream[_] => new ordered.FilterStream(os.element, os.value) with Counted[T]
//
//    case _ => throw new RuntimeException("Dont know how to convert to Counted: " + streamable)
//  }
//  
//}