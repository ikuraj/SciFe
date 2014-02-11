package insynth.streams
package dependent

class InMapper[NewIn, I, +O](inner: Dependent[I, O], f: NewIn => I)
	extends Dependent[NewIn, O] {

  def getStream(parameter: NewIn) =
    inner.getStream( f(parameter) )
  
}