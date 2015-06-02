//package scife.enumeration
//package dependent
//package combinators
//
//import scala.language.higherKinds
//
//trait Map[I, O, O2] extends Depend[I, O2] {
//
//  type DependType <: Depend[I, O]
//  override type EnumType <: Enum[O2]
//
//  val inner: DependType
//  val f: O => O2
//
//  override def getEnum(parameter: I) =
//    inner.getEnum( parameter ) map f
//
//}
