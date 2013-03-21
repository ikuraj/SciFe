package insynth.streams

trait Valuable[T] {
  def getValues: Stream[T]
}