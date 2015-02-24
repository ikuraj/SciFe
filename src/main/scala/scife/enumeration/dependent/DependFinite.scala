package scife.enumeration
package dependent

import scala.language.higherKinds

trait DependFinite[I, O] extends Depend[I, O] {

  override type EnumSort[A] <: Finite[A]

  // concatenation
//  def concat(e: DependFinite[I, O]): DependFinite[I, O] =
//    Concat(this, e)
//
//  def ++[O2](e: Depend[I, O2]) = concat(e)
//  def ⊕[O2](e: Depend[I, O2]) = concat(e)

  // products
  def product[O2](e: DependFinite[I, O2]): DependFinite[I, (O, O2)] =
    Product(this, e)

  def **[O2](e: DependFinite[I, O2]) = product(e)
  def ⊗[O2](e: DependFinite[I, O2]) = product(e)

  // inmap
  override def inmap[I2](f: PartialFunction[I2, I]): DependFinite[I2, O] = {
    InMapP(this, f)
  }

  override def ↓[I2](f: PartialFunction[I2, I]) = inmap(f)

  // chain
  def chain(e: Finite[I]): Finite[(I, O)] =
    Chain(e, this)

  def ⊘(e: Finite[I]) = chain(e)
    
  def chainSingle(e: Finite[I]): Finite[O] =
    Chain.single(e, this)

}
