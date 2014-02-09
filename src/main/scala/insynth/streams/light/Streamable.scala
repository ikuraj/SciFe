//package insynth.streams.light
//
//import insynth.util.logging.HasLogger
//
///**
// * Streamable represents a graph that encodes how are instances constructed - these
// * instances can be enumerated from a stream
// * NOTE: Graph may encode recursive constraints but each streamable must be able to
// * enumerate elements without special queries, such as the availability of a
// * particular element
// * @param <T> Type of enumerated elements
// */
//trait StreamIterable[+T] {
//  
//  def getIterator: Iterator[T]
//  
//}