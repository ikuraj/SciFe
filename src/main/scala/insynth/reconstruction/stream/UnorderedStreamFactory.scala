package insynth.reconstruction.stream

import insynth.streams.Streamable
import insynth.streams.unordered._

class UnorderedStreamFactory[T] extends StreamFactory[T] {
  
  override def makeEmptyStreamable = Empty

  override def makeSingleton[U <: T](element: U) = Singleton(element)
  
  override def makeSingletonList[U <: T](element: List[U]) = Singleton(element)
  
  override def makeSingleStream[U <: T](stream: => Stream[(U, Int)]) =
    SingleStream(stream map (_._1), true)

  override def makeFiniteStream[U <: T](array: => Vector[(U, Int)]) =
    SingleStream(array.toStream map (_._1), false)
  
  override def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X=>Y, modifyVal: Option[Int => Int] = None) =
    UnaryStream(streamable, modify)
  
  override def makeUnaryStreamList[X, Y <: T](streamable: Streamable[X], modify: X => List[Y]) =
    UnaryStream(streamable, modify)
    
  override def makeFilterStream[U <: T](streamable: Streamable[U], filterFun: U => Boolean) =
    FilterStream(streamable, filterFun)
  
  override def makeBinaryStream[X, Y, Z <: T](s1: Streamable[X], s2: Streamable[Y])(combine: (X, Y) => List[Z]) =
    BinaryStream(s1, s2)(combine)
  
  override def makeRoundRobbin[U <: T](streams: Seq[Streamable[U]]) =
    RoundRobbin(streams)
  
  override def makeLazyRoundRobbin[U <: T](initStreams: Seq[Streamable[U]]) =
    LazyRoundRobbin(initStreams.toList)
      
  override def makeLazyRoundRobbinList[U <: T](initStreams: Seq[Streamable[List[U]]]) =    
    LazyRoundRobbin(initStreams.toList)
        
  def getFinalStream(streamable: Streamable[T]) =
    streamable.getStream zip Stream.continually(0f)
  
}