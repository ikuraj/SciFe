package scife
package enumeration

import _root_.scife.util.Math._

class SublistsOfSize[T](e: Enum[List[T]], collSize: Int, subSize: Int) extends Finite[List[T]] {

  override def size = Binomial.binomialCoefficient(collSize, subSize)

  override def apply(ind: Int) = e(ind)

}

object Sublists {
  
  import scife.{ enumeration => e }
  import scife.enumeration.dependent._
  
  private def subListChooser[T]: DependFinite[(Int, List[T]), List[T]] = Depend.memoizedFin(
    (self: DependFinite[(Int, List[T]), List[T]], pair: (Int, List[T])) => {
      require(pair._1 >= 1, s"size is ${pair._1}")
      val (size, range) = pair
      
      if (size == 1) Enum(range map { List(_) })
      else if (range.isEmpty) Nil
      else if (size == range.size)
        Singleton(range)
      else {  
        val temp = self.getEnum( (size - 1, range.tail) )
        val kept = Map( temp , { range.head :: (_: List[T]) })
        val leftOut = self.getEnum( (size, range.tail) )
  
        val allNodes = e.Concat(kept, leftOut)
        assert(range.size > 0)
        new SublistsOfSize(allNodes, range.size, size ): Finite[List[T]]
      }
    })
  
  def apply[T](coll: List[T], subSize: Int): Finite[List[T]] =
    subSize match {
      case 0 => Singleton( Nil )
      case n if n < coll.size =>
        subListChooser( (subSize, coll) )
      case n if coll.size == n =>
        Singleton(coll)
        //new SublistsOfSize(coll, subSize)
        
        // handled by subListChooser
//      case 1 if coll.size >= 1 => Enum(coll map { List(_) })
      case _ => Empty
    }
  
//  def apply[T](coll: Traversable[T], subSize: Int): Finite[Traversable[T]] = this(coll.toList, subSize)

  def apply[T](coll: List[T]): Finite[List[T]] = coll match {
    case Nil =>
      Singleton(Nil)  
    case x :: Nil =>
      Enum( Nil, List(x) )
    case x :: y :: Nil =>
      Enum( Nil, List(x), List(y), List(x, y))
    case _ =>
      val dep: DependFinite[Int, List[T]] = Depend.fin(
        (subSize: Int) => (this(coll, subSize): Finite[List[T]])
      )
  
      e.Concat(
        this(coll, 0),
        Map( Enum(1 until coll.size) ⊘ dep, { p: (Int, List[T]) => p._2 } ),
        this(coll, coll.size)
      )
  }
    
}
