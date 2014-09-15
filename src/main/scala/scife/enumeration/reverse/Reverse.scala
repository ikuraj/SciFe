package scife.enumeration
package reverse

import util.EnumStream

import scala.reflect._
import scala.language.implicitConversions

// knows how to reverse but needs to have the input parameter I specified
trait ReverseT[+A] {
  
//  def reverse[B >: A](a: B, parameter: I): Finite[A]
  
  def reverse[B >: A](a: B): Int
  
}

//trait ReverseT[+A] {
//  
//  def reverse[B >: A](a: B): Finite[A]
//  
////  def reverse[B >: A](a: B): Int
//  
//}

trait Reverse[+O] extends Finite[O] with ReverseT[O] {
  
  // TODO do this with pattern matching
//  def reverse[B >: A](a: B): Finite[A] =
//    Reverser(this, a.asInstanceOf[A])
  
}