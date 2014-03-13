package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck.Gen

import streams.{ light => e }

import util._
import util.format._
import util.logging._
import common._

class SortedListEqualTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Inspectors with
	HasLogger with ProfileLogger {  
  import Checks._
  import Structures._

  def tests(lists: Dependent[(Int, Int), List[Int]]) {
    var elements: Iterable[List[Int]] = null
    def clue = elements.mkString("\n")
    
    withLazyClue("Elements are: " + clue) {
    
//	    {
//	      val en = lists.getStream(1, 2)
//	      elements = for (ind <- 0 until en.size) yield en(ind)
//	      en.size should be (2)
//	    }
//    
//	    {
//	      val en = lists.getStream(2, 3)
//	      elements = for (ind <- 0 until en.size) yield en(ind)
//	      en.size should be (3)
//	    }
//    
//	    for (size <- 1 to 10) {
//	      val en = lists.getStream(size, size + 1)
//	      elements = for (ind <- 0 until en.size) yield en(ind)
//	      elements.size should be ( Binomial.binomialCoefficient(size + 1, size) )
//	    }
    
	    {
	      val en = lists.getStream(11, 11)
	      elements = for (ind <- 0 until en.size) yield en(ind)
	      en.size should be (352716)
	    }

//      for (size <- 11 to 10) {        
//        elements = lists.getStream((size, size)).toList
//        
//        forAll (elements) { lst =>
//          forAll(lst.zip(lst.tail)) {
//            case ((i, j)) => 
//              assert( i > j )
//          }
//        }
//        elements.size should be ( Binomial.binomialCoefficient(size, size + 1) )
//      }
      
      forAll( Gen.choose(1, 10), Gen.choose(1, 10), maxSize(50) ) { (size: Int, m: Int) =>
        whenever(true) {        
          elements = lists.getStream((size, m)).toList
          
          forAll (elements) { lst =>
            forAll(lst.zip(lst.tail)) {
              case ((i, j)) => 
                assert( i >= j )
            }
          }
        }
      }
    }
    
  }
  
  def construct = {
    
    val naturals = Producer[Int, Int](
      (range: Int) => {
        e.WrapperArray( 1 to range )
      }
    )
    
    var getListOfSize: Dependent[ (Int, Int), List[Int] ] = null
    
    // 
    val treesOfSize: Dependent[ (Int, Int), List[Int] ] = Producer.memoized(
      ( pair: (Int, Int) ) => {
        val (size, max) = pair
        info("got into construction with, size=%d, max=%d".format(size, max))
        assert(size >= 0, "size=%d, max=%d" format (size, max))

        if (size == 0) e.Singleton( Nil )
        else if (size > 0) {
          val roots = naturals.getStream(max)
          
          val innerLists: Dependent[Int, List[Int]] = new InMapper(getListOfSize, { (par: Int) =>
            (size - 1, par)
          })
          
          import BinaryFiniteMemoized._
          
          val allLists =
            combine[Int, List[Int], List[Int]](roots, innerLists,
              (head: Int, l: List[Int]) => {
                head :: l
              }
            )
          
          allLists
        } else e.Empty
      }
    )
    
    getListOfSize = treesOfSize
    
    treesOfSize
  }

  test("sorted lists") {
    
    tests( construct )
    
  }
  
}
