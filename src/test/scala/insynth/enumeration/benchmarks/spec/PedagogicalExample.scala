package insynth
package enumeration
package benchmarks
package spec

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

import Structures._
import BSTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.existentials

class PedagogicalExample extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import common._
  import Util.CheckerHelper
  import Checks._
  
//  test("correctness") {
//    val ms = new MemoizationScope
//    val enum = constructEnumerator(ms)
//    ms.memoizations.size should be (1)
//    
//    val helper = new CheckerHelper[Tree]
//    import helper._
//    
//    withLazyClue("Elements are: " + clue) {
//    
//    val profileRange = 1 to 15
//
//	    for (size <- profileRange) {
//	      ms.clear
//	      profile("Getting stream for BST of size %d".format(size)) {
//	        res = enum.getEnum(size, 1 to size)
//	      }
//	      profile("Claculating size for BST of size %d".format(size)) {
//	        res.size should be (Catalan.catalan(size))
//	      }
//	      profile("Getting elements for BST of size %d".format(size)) {
//	        for (ind <- 0 until res.size) res(ind)
//	      }
//	      
//	      assert( (for (ind <- 0 until res.size) yield res(ind)).forall( invariant(_) ) )
//	    }
//
//    }
//  }

  // paper

//val allConvenientlyDivisblePairs =
// ( List( (1, 1), (3, 3) ) ++
//  ((1 to 9) ** (Stream.from(0) filter { _ % 5 == 0 })
// ) map { case (x, y) => ("valid", x, y) }
//\end{lstlisting}
  
  
  def constructEnumerator1(implicit ms: MemoizationScope) = {
    import e._
    import Enum._
   
//    (Enum( (1, 1), (3, 3) ): Enum[(Int, Int)] ) ⊕ ( Enum(1 to 9)
//      product (Stream.from(0) ⊘ { _ % 5 == 0 })
//    ) ↑ { case (x, y) => ("valid", x, y) }

  }

  def constructEnumerator2(implicit ms: MemoizationScope) = {
    import e._
    import Enum._
//   
//    val enum = rec(self, ind) {
//      Enum(2, 3) concat
//        inmap(ind - 2)self 
//    }

//    val res: Enum[(String, ((Int, Int), Int))] =
//    ((Enum(1 to 31) ** Enum(1 to 12) ** Enum(Stream.from(2014)) ⊘ {
//      _ => true; // whether its a good year
//    }): Enum[((Int, Int), Int)]) ↑ { case p@((x, y), z) => if (true) ("leap", p) else ("no", p) }
//    
//
//    val res2 =
//    (1 to 31) ⊗ (1 to 12) ⊗ Stream.from(2014) ⊘ {
//      case ((d, m), y) => true } ↑ { ("leap", _) }
//    
//    
//    // lazy fibonacci are doable!
//    val fibs: Enum[Int] = (Enum(0, 1): Enum[Int])
//    val fibs2: Enum[Int] = (Enum(Stream.from(0)).map{ (i: Int) => fibs(i-1) + fibs(i-2) })
//    val fib = fibs ++ fibs2
  }


}
