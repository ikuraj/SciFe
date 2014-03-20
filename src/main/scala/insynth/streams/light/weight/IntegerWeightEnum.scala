package insynth.streams
package light

package object weight {
  type IntegerWeightEnum[@specialized T] = WeightEnum[T, Int]
  
  type IntegerWeightPair[T] = (T, Int)
  
  type FiniteIntegerWeightEnum[T] = WeightEnum[T, Int] with Finite[(T, Int)]
  
  type InfiniteIntegerWeightEnum[T] = WeightEnum[T, Int] with Infinite[(T, Int)]
}