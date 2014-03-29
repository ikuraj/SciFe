package insynth.enumeration

object Product {
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V]) =
	  lzy.Product(s1, s2)
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V], combine: (T, V) => U) =
	  lzy.Product(s1, s2, combine)
  
}