package insynth.enumeration

case class WrapFunction[T](val f: Int => T) extends Infinite[T] {
  
  override def apply(ind: Int) = f(ind)
  
}

class WrapFunctionWithDomain[T](val f: Int => T, inSize: Int) extends Finite[T] {
  
  def this(f: Int => T, domain: Range) = {
    this(
      // check if index is in the domain and then apply
      { (ind: Int) =>
        if (ind < domain.start || ind > domain.end)
          f(ind)
        else throw new IllegalArgumentException("Index not in the domain of this enumerator.")
      }
    , domain.size)
  }
  
  override def size = inSize
  
  override def apply(ind: Int) = f(ind)
  
}