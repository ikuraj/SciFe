package scife.enumeration
package iterable
package lzy

import scife.enumeration.dependent._

package object dependent {
  
  type LazyDepend[I, O] = Depend[I, O] { type EnumSort[A] = LazyEnum[A] }
  
  type LazyDependFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = LazyEnumFinite[A] }

}