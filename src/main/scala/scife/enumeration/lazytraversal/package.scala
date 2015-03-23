package scife.enumeration

import scife.enumeration.dependent._

package object lazytraversal {

  type LazyEnum[+A] = Enum[A] with Touchable[A] with Skippable[A]

  type LazyEnumFinite[+A] = Finite[A] with Touchable[A] with Skippable[A]

  type TouchableEnum[+A] = Finite[A] with Touchable[A]
  
  type LazyDepend[I, O] = Depend[I, O] { type EnumSort[A] = LazyEnum[A] }
  
  type LazyDependFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = LazyEnumFinite[A] }

}