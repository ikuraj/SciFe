package insynth.enumeration
package dependent
package combinators

trait InMap[I, +O, NewIn] extends Depend[NewIn, O] {
  
  override type EnumType = inner.EnumType
  
  val inner: Depend[I, O]
  val f: NewIn => I
  
  override def getEnum(parameter: NewIn) =
    inner.getEnum( f(parameter) )

}