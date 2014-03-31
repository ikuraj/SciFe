package insynth.enumeration

object Product {
  
	def apply[T, V](s1: Enum[T], s2: Enum[V]) =
	  lzy.Product(s1, s2)
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V], combine: (T, V) => U) =
  	lzy.Product(s1, s2, combine)
  
	def apply[T, V](s1: Finite[T], s2: Finite[V]): Finite[(T, V)] =
	  new lzy.ProductFinite(s1, s2)
  
	def apply[T, V, U](s1: Finite[T], s2: Finite[V], combine: (T, V) => U): Finite[U] =
  	new lzy.ProductFiniteComb(s1, s2)(combine)
  
}