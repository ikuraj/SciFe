package insynth.enumeration
package reverse

import util.EnumStream

import scala.reflect._
import scala.language.implicitConversions

trait ReverseT[+A] {
  
  def reverse[B >: A](a: B): Finite[A]
  
//  def reverse[B >: A](a: B): Int
  
}

trait Reverse[+A] extends Finite[A] with ReverseT[A] {
  
  // TODO do this with pattern matching
//  def reverse[B >: A](a: B): Finite[A] =
//    Reverser(this, a.asInstanceOf[A])
  
}