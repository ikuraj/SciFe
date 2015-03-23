package scife.enumeration
package lazytraversal

import combinators.{ Product => CProduct }

import scalaz.LazyTuple2

protected[enumeration] class ProductFiniteLazyTuple[T, V]
  (val left: TouchableEnum[T], val right: TouchableEnum[V])
  extends Finite[LazyTuple2[T, V]] with Skippable[LazyTuple2[T, V]] with Resetable[LazyTuple2[T, V]] {
  
//  require(left.size > 0, "at " + this.hashCode())
//  var leftOffset = 0
//  var rightOffset = 0
  
  val skipMap = //scala.collection.mutable.Map[Int, Int]()//.default(-1)
    Array.fill(left.size+1)(right.size)
    // last one is sentinel
  
  override def next(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    
    if (left.touched && right.touched)
      ind + 1
    else if (!left.touched && right.touched) {
      println(s"left $left, not touched")
      i2 * left.size + left.size
    }
    else if (left.touched && !right.touched) {
//      println("rigjt not touched")
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
        val newRow = skipMap.indexWhere( newColumn < skipMap(_) )

        if (newRow < left.size) newColumn * left.size + newRow
        else size
      }
    }
    else {
//      println("both not touched")
 
      size
    }
  }
  
  override def apply(ind: Int) =
  {
    entering("apply", ind)
    val i1 = ind % left.size
    val i2 = ind / left.size
//    left.touched = false
//    right.touched = false
    
    println(s"invoke(untouch) from ${this.hashCode} for " +
      s"${left.hashCode}/$i1, ${right.hashCode}/$i2")
//    val leftO = leftOffset
//    val rightO = rightOffset
//    println(s"left/right offest: $i1/$i2")
//    val leftV = left(i1)
//    val rightV =right(i2) 
//    LazyTuple2(leftV, rightV)
    LazyTuple2(
//      { val res = left(i1); left.touched = true; res},
//      { val res = right(i2); right.touched = true; res}
      { left(i1) },
      { right(i2) }
    )
  }
  
  override def reset = {
    left.reset
    right.reset
  }

  override def size = left.size * right.size
  
  override def toString =
    s"ProductFiniteLazyTuple[$hashCode]"
  
}

protected[enumeration] class ProductFiniteStrictTuple[T, V]
  (val left: TouchableEnum[T], val right: TouchableEnum[V])
  extends Finite[(T, V)] with Skippable[(T, V)] with Resetable[(T, V)] {
  
//  require(left.size > 0, "at " + this.hashCode())
//  var leftOffset = 0
//  var rightOffset = 0
  
  val skipMap = //scala.collection.mutable.Map[Int, Int]()//.default(-1)
    Array.fill(left.size+1)(right.size)
    // last one is sentinel
  
  override def next(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    
    if (left.touched && right.touched)
      ind + 1
    else if (!left.touched && right.touched)
      i2 * left.size + left.size
    else if (left.touched && !right.touched) {
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
        val newRow = skipMap.indexWhere( newColumn < skipMap(_) )

        if (newRow < left.size) newColumn * left.size + newRow
        else size
      }
    }
    else size
  }
  
  override def apply(ind: Int) =
  {
    entering("apply", ind)
    val i1 = ind % left.size
    val i2 = ind / left.size
    
//    val leftO = leftOffset
//    val rightO = rightOffset
//    println(s"left/right offest: $i1/$i2")
    val leftV = left(i1)
    val rightV =right(i2) 
    (leftV, rightV)
//    LazyTuple2(left(i1), right(i2))
  }

  override def size = left.size * right.size
  
  override def reset = {
    left.reset
    right.reset
  }
  
}