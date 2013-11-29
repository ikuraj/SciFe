package insynth.reconstruction.stream

import insynth.streams.{ Streamable, AddStreamable }

trait StreamFactory[T] {
  
  def makeEmptyStreamable: Streamable[T]

  def makeSingleton[U <: T](element: U): Streamable[T]
  
  def makeSingletonList[U <: T](element: List[U]): Streamable[List[T]]
  
  def makeSingleStream[U <: T](stream: => Stream[U]): Streamable[T]

  def makeFiniteStream[U <: T](array: => Vector[U]): Streamable[T]
  
  def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X => Y, modifyVal: Option[Int => Int] = None): Streamable[T]
  
  def makeUnaryStreamList[X, Y <: T](streamable: Streamable[X], modify: X => List[Y]): Streamable[List[T]]
  
  def makeBinaryStream[X, Y, Z <: T](s1: Streamable[X], s2: Streamable[Y])(combine: (X, Y) => List[Z]): Streamable[List[T]]
  
  def makeRoundRobbin[U <: T](streams: Seq[Streamable[U]]): Streamable[T]
  
  def makeLazyRoundRobbin[U <: T](initStreams: List[Streamable[U]]): Streamable[T] with AddStreamable[T]

  def makeLazyRoundRobbinList[U <: T](initStreams: List[Streamable[List[U]]]): Streamable[List[T]] with AddStreamable[List[T]]
  
  def getFinalStream(streamable: Streamable[T]): Stream[(T, Float)]
  
}