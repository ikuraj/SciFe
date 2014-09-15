package scife.enumeration

object Identity extends Infinite[Int] {
  
  override def apply(ind: Int) = {
    ind
  }
  
}

class IdentitySize(_size: Int) extends Finite[Int] {
  
  override def size = _size
  
  override def apply(ind: Int) = {
    ind
  }
  
}

class WrapRange(range: Range) extends Finite[Int] {
  
  override def size = range.size
  
  override def apply(ind: Int) = range(ind)
  
}