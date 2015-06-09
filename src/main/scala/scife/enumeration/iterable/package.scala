package scife.enumeration

package object iterable {

  type ResetIterFinite[+A] = Finite[A] with ResetIter[A]
  
}