package scife.util
package test

import java.util.concurrent._
import atomic._
import java.io.{File}

import util._
import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.specs2.mutable._
import org.specs2.matcher

import scala.language.existentials

class CompilerMatcherTest extends TestBase with HasLogger with ProfileLogger {
  
  "empty program" in {
    
    "class X"::Nil must compileString (outdir = "./tmp", usecurrentcp = true)
    
  }
  
  "another example" in {
    """class Tuple2Int(val encoding: Long) extends AnyVal with Product2[Int, Int] {
      def canEqual(that: Any) = false
      def _1: Int = 0
      def _2: Int = 0
    }""" :: Nil must compileString (outdir = "./tmp", usecurrentcp = true)
  }
  
  "errorneous example should not compile" in {
    """trait Functor[F[_]] {
        def map[A, B](fa: F[A])(f: A => B): F[B]
      }
       
      final case class Mu[F[_]](value: F[Mu[F]]) extends AnyVal {
        def cata[B](f: F[B] => B)(implicit F: Functor[F]): B =
          f(F.map(value)(_.cata(f)))
      }""" :: Nil must compileString(outdir = "./tmp", usecurrentcp = true)
  }

}

trait TestBase extends Specification with CompilerMatcher {
  val tmp = new File("tmp")
  if (tmp.exists) deleteAll(tmp)
  tmp.mkdirs() // you need this for copyFileFromResource
}
