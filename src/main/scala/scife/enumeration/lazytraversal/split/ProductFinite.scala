package scife.enumeration
package lazytraversal
package split

import combinators.{ Product => CProduct }

import scalaz.LazyTuple2

protected[enumeration] class ProductFiniteLazyTuple[T, V]
  (val left: TouchableEnum[T], val right: TouchableEnum[V])
  (_inner: ProductFiniteLazyTupleInner[T, V] = null)
  extends Finite[LazyTuple2[T, V]] with Skippable[LazyTuple2[T, V]] with Resetable[LazyTuple2[T, V]] {
  
  def this(_in: ProductFiniteLazyTupleInner[T, V]) =
    this(_in.left, _in.right)(_in)
  
  val inner =
    if (_inner == null)
      new ProductFiniteLazyTupleInner(left, right)
    else _inner
  
  val skipMap = //scala.collection.mutable.Map[Int, Int]()//.default(-1)
    Array.fill(left.size+1)(right.size)
    // last one is sentinel
  
  override def next(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    
    if (left.touched && right.touched) {
//      println("both touched")
      ind + 1
    }
    else if (!left.touched && right.touched) {
      println(s"left $left, not touched")
      i2 * left.size + left.size
    }
    else if (left.touched && !right.touched) {
      println("rigjt not touched")
      // add vertical skip
      skipMap( i1 ) = i2
      var row = i1
      while ( i2 >= skipMap(row) ) {
        // while rows above are skipped
        row += 1
      }
      
//      println(s"ind=$ind, i1=$i1, i2=$i2, row=$row, skipMap=${skipMap.mkString(",")}")
      if (row < left.size)
        i2 * left.size + row      
      else {
        val newColumn = i2 + 1
        val newRow = skipMap.indexWhere( newColumn < _ )
//        println(s"newColumn=$newColumn, newRow=$newRow")

        if (newRow >= 0) newColumn * left.size + newRow
        else size
      }
    }
    else {
//      println("both not touched")
 
      size
    }
  }
  
  override def apply(ind: Int) =
    inner.apply(ind)
  
  override def reset = {
    super.reset
    if (left.touched) left.reset
    if (right.touched) right.reset
//    left.reset
//     right.reset
//    for (i <- 0 until skipMap.size) skipMap(0) = right.size
  }
  
  override def size = inner.size

}