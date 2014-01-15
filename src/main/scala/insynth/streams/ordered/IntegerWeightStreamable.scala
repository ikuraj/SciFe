package insynth.streams

package object ordered {
  type IntegerWeightStreamable[T] = OrderedStreamable[T, Int]
  
  type IntegerWeightPair[T] = (T, Int)
}