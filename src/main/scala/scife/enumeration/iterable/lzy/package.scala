package scife.enumeration
package iterable

package object lzy {

  type LazyEnum[+A] = Enum[A] with ResetIter[A] with Touchable[A]

  type LazyEnumFinite[+A] = Finite[A] with ResetIter[A] with Touchable[A]

}