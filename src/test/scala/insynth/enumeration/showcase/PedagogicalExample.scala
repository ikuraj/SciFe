package insynth
package enumeration
package showcase

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
  
  // import DSL
  import e._
  import Enum._
  
//  test("Dates example") {
//    val dates = (1 to 31) ⊗ (1 to 12) ⊗ Stream.from(2014) 
//      { isValid( ) } ↑ { case ((d, m), y) ⇒ new Date(y, m, d) }
//
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
