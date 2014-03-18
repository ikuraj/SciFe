package insynth.streams
package light

import scala.language.implicitConversions

import org.scalatest._

class CombinatorsTest extends FunSuite with ShouldMatchers {
  
  import Enum._
  
  test("example from the paper, fine grain checks") {
    val listEnum: Enum[(Int, Int)] = List( (1, 1), (3, 3) )
    val rangeEnum: Enum[Int] = (1 to 5)
    val streamEnum: Enum[Int] = Stream.from(0)
    
    listEnum shouldBe a [Finite[_]]
    rangeEnum shouldBe a [Finite[_]]
    streamEnum shouldBe a [Infinite[_]]
    
    val productEnum = rangeEnum ** streamEnum
    
    productEnum shouldBe a [BinaryOneFinite[_, _, _]]
    
    val concatEnum = listEnum ++ productEnum
    
    concatEnum shouldBe a [RoundRobbinMixedSingle[_]]
    
    val allConvenientlyDivisblePairs =
			( listEnum ++
			(rangeEnum ** streamEnum)
			) map { case (x, y) => ("valid", x, y) }
    
    allConvenientlyDivisblePairs shouldBe a [Mapper[_, _]]
    
    allConvenientlyDivisblePairs.take(200) should contain allOf (
      ("valid", 1, 1), ("valid", 3, 3), ("valid", 3, 2), ("valid", 5, 3)
    )    
  }
  
  test("example from the paper, with filter") {
    val listEnum: Enum[(Int, Int)] = List( (1, 1), (3, 3) )
    val rangeEnum: Enum[Int] = (1 to 5)
    val streamEnum: Enum[Int] = Stream.from(0) filter { _ % 5 == 0 }
    
    listEnum shouldBe a [Finite[_]]
    rangeEnum shouldBe a [Finite[_]]
    streamEnum shouldBe a [Infinite[_]]
    
    val productEnum = rangeEnum ** streamEnum
    
    productEnum shouldBe a [BinaryOneFinite[_, _, _]]
    
    val concatEnum = listEnum ++ productEnum
    
    concatEnum shouldBe a [RoundRobbinMixedSingle[_]]
    
    val allConvenientlyDivisblePairs =
      ( listEnum ++
      (rangeEnum ** streamEnum)
      ) map { case (x, y) => ("valid", x, y) }
    
    allConvenientlyDivisblePairs shouldBe a [Mapper[_, _]]

    allConvenientlyDivisblePairs.take(200) should contain noneOf (
      ("valid", 2, 7), ("valid", 2, 4), ("valid", 1, 2), ("valid", 1, 3)
    )    
    
    allConvenientlyDivisblePairs.take(200) should contain allOf (
      ("valid", 1, 10), ("valid", 4, 25), ("valid", 3, 25), ("valid", 5, 35)
    )    
  }
  
//  test("example from the paper, with explicit filter") {
//    val listEnum: Enum[(Int, Int)] = List( (1, 1), (3, 3) )
//    val rangeEnum: Enum[Int] = (1 to 5)
//    val streamEnum: Enum[Int] = Enum(Stream.from(0)) filter { _ % 5 == 0 }
//    
//    listEnum shouldBe a [Finite[_]]
//    rangeEnum shouldBe a [Finite[_]]
//    streamEnum shouldBe a [Infinite[_]]
//    
//    val productEnum = rangeEnum ** streamEnum
//    
//    productEnum shouldBe a [BinaryOneFinite[_, _, _]]
//    
//    val concatEnum = listEnum ++ productEnum
//    
//    concatEnum shouldBe a [RoundRobbinMixedSingle[_]]
//    
//    val allConvenientlyDivisblePairs =
//      ( listEnum ++
//      (rangeEnum ** streamEnum)
//      ) map { case (x, y) => ("valid", x, y) }
//    
//    allConvenientlyDivisblePairs shouldBe a [Mapper[_, _]]
//
//    allConvenientlyDivisblePairs.take(200) should contain noneOf (
//      ("valid", 2, 7), ("valid", 2, 4), ("valid", 1, 2), ("valid", 1, 3)
//    )    
//    
//    allConvenientlyDivisblePairs.take(200) should contain allOf (
//      ("valid", 1, 10), ("valid", 4, 25), ("valid", 3, 25), ("valid", 5, 35)
//    )    
//  }
//  
//  test("example from the paper, unicode formulation") {
//    val allConvenientlyDivisblePairs =
//      ( List((1, 1), (3, 3)) ) ⊕
//        ( (1 to 9) ⊗ (Stream.from(0) filter { _ % 5 == 0 })) map { case (x, y) => ("valid", x, y) }
//    
//    allConvenientlyDivisblePairs shouldBe a [Mapper[_, _]]
//    
//    allConvenientlyDivisblePairs.take(200) should contain allOf (
//      ("valid", 1, 1), ("valid", 3, 3), ("valid", 3, 2), ("valid", 5, 3)
//    )    
//  }

}