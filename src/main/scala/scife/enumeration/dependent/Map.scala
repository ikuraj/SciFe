//package scife.enumeration
//package dependent
//
//import scala.language.higherKinds
//
//class Map[I, O, O2, DependIn[I, +O] <: Depend[I, O]](
//  override val inner: DependIn[I, O], override val f: O => O2
//) extends combinators.Map[I, O, O2] {
//
//  override type DependType = DependIn[I, O]
//
//}
//
//object Map {
//
//  def apply[I, O, O2](tde: Depend[I, O], modify: O => O2) =
//    tde match {
//      case f: DependFinite[I, O] =>
//        new Map(f, modify) with DependFinite[I, O2]
//      case i: DependInfinite[I, O] =>
//        new Map(i, modify) with DependInfinite[I, O2]
//    }
//
//  def apply[I, O, O2](tde: DependFinite[I, O], modify: O => O2) =
//    new Map(tde, modify) with DependFinite[I, O2]
//
//  def apply[I, O, O2](tde: DependInfinite[I, O], modify: O => O2) =
//    new Map(tde, modify) with DependInfinite[I, O2]
//}