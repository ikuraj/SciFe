package scife.enumeration
package lazytraversal
package split

import combinators.{ Product => CProduct }

import scalaz.LazyTuple2

protected[enumeration] class ProductFiniteLazyTupleInner[T, V]
  (val left: TouchableEnum[T], val right: TouchableEnum[V])
  extends Finite[LazyTuple2[T, V]] {
  
  override def apply(ind: Int) =
  {
    val i1 = ind % left.size
    val i2 = ind / left.size
//    left.touched = false
//    right.touched = false
    
//    println(s"invoke(untouch) from ${this.hashCode} for " + s"${left.hashCode}/$i1, ${right.hashCode}/$i2")
//    val leftO = leftOffset
//    val rightO = rightOffset
//    println(s"left/right offest: $i1/$i2")
//    val leftV = left(i1)
//    val rightV =right(i2) 
//    LazyTuple2(leftV, rightV)
    LazyTuple2(
      { val res = left(i1); left._touched = true; res},
      { val res = right(i2); right._touched = true; res}
//      { left(i1) },
//      { right(i2) }
    )
  }
  
  override def size = left.size * right.size
  
  override def toString =
    s"ProductFiniteLazyTuple[$hashCode]"
  
}