package insynth.enumeration

object Identity extends Infinite[Int] {
  
  override def apply(ind: Int) = {
    import Bla._
    if (i>1000) throw new RuntimeException
    i+= 1
    ind
  }
  
}

object Bla {
  var i = 0
}

class IdentitySize(_size: Int) extends Finite[Int] {
  
  override def size = _size
  
  override def apply(ind: Int) = {
    import Bla._
    if (i>1000) throw new RuntimeException
    i+= 1
    ind
  }
  
}

class WrapRange(range: Range) extends Finite[Int] {
  
  override def size = range.size
  
  override def apply(ind: Int) = range(ind)
  
}