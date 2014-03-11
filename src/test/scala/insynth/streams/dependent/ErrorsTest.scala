//package insynth
//package streams
//package light
//
//import scala.collection.mutable
//
//import org.scalatest._
//import org.scalatest.matchers._
//
//class ErrorsTest extends FunSuite with ShouldMatchers {
//
//  trait Enumerable[+A] {
//    
//    def size: Int
//    
//  }
//  
//  trait Finite[+A] extends Enumerable[A] {
//    
//    self: Enumerable[A] =>
//    
//    val memoizedSize: Int = self.size
//    
//    override def size = memoizedSize
//    
//  }
//  
//  case class WrapperArray[T](coll: IndexedSeq[T])
//    extends Finite[T] {
//    
//    override def size = coll.size
//    
//  }
//
//  abstract class RoundRobbinFinite[T] protected[streams] ()
//    extends Finite[T] {
//  
//    override def size: Int
//  
//  }
//  
//  class RoundRobbinFiniteFixed[/*@specialized */T] protected[streams] (val streams: IndexedSeq[Enumerable[T]])
//    extends RoundRobbinFinite[T] {
//  
//    val limits =
//      mutable.IndexedSeq(0)
//    assert(limits != null && limits.size == streams.size + 1)
//  
//    override def size = {
//      assert(limits != null)
//      limits.apply(0)
//    }
//    
//  }
//  
//  test("Simple accesses, fixed") {
//    val arrays: Array[Enumerable[Int]] = Array(
//      WrapperArray(Array(1, 2, 3)),
//      WrapperArray(Array(4, 5, 6)),
//      WrapperArray(Array(7, 8, 9))
//    )
//    arrays.size should be (3)
//    
//    val rr = new RoundRobbinFiniteFixed[Int](arrays)
//    
//    rr.size should be (9)
//  }
//}