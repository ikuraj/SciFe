package scife.enumeration
package iterable

object Product {

  def apply[T, V](s1: ResetIter[T], s2: ResetIter[V]): Finite[(T, V)] with ResetIter[(T, V)] =
    (s1, s2) match {
//      case (Empty, _) | (_, Empty) =>
//        Empty
//      case (s1: Singleton[T], s2: Finite[V]) =>
//        new ProductSingleton(s1, s2)
//      case (s1: Finite[T], s2: Singleton[V]) =>
//        new ProductSingletonRight(s1, s2)
      case (s1: Finite[T], s2: Finite[V]) =>
        new ProductFinite(s1, s2)
    }
  
  def apply[T, V](s1: ResetIterFinite[T], s2: ResetIterFinite[V]): ResetIterFinite[(T, V)] =
    (s1, s2) match {
//      case (Empty, _) | (_, Empty) =>
//        Empty
//      case (s1: Singleton[T], s2: Finite[V]) =>
//        new ProductSingleton(s1, s2)
//      case (s1: Finite[T], s2: Singleton[V]) =>
//        new ProductSingletonRight(s1, s2)
      case (s1: ResetIterFinite[T], s2: ResetIterFinite[V]) =>
        new ProductFinite(s1, s2)
    }

//  def apply[T, V, U](s1: Finite[T], s2: Finite[V], combine: (T, V) => U): Finite[U] =
//    (s1, s2) match {
//      case (Empty, _) | (_, Empty) =>
//        Empty
//      case (s1: Singleton[T], s2: Finite[V]) =>
//        Map(s2, { (v: V) => combine(s1.el, v) })
//      case (s1: Finite[T], s2: Singleton[V]) =>
//        Map(s1, { (v: T) => combine(v, s2.el) })
//      case (s1: Finite[T], s2: Finite[V]) => new ProductFiniteComb(s1, s2)(combine)
//    }

}
