package scife
package enumeration

import org.paukov.combinatorics.Factory

//import org.scalatest._
//import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import scala.collection.JavaConversions._

//class SublistsTest extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {
object SublistsTest extends Properties("Sublists") {

  val listGen = for {
    size <- Gen.oneOf(1 to 15)
    list <- Gen.containerOfN[List, Int](size, Gen.oneOf(1 to 10))
  } yield list

  //  test("integer lists") {
  property("size of sublist of integer lists") =
    forAll(listGen) { (list: List[Int]) =>
      val enum = Sublists(list)

      // Create an initial vector/set
      val initialSet = Factory.createVector(list)

      // Create an instance of the subset generator
      val gen = Factory.createSubSetGenerator(initialSet, false)

      //        enum.size shouldBe gen.getNumberOfGeneratedObjects
      enum.size == gen.getNumberOfGeneratedObjects
    }

  property("elements of sublists of itneger lists") =
    forAll(listGen) { (list: List[Int]) =>

      val enum = Sublists(list)

      // Create an initial vector/set
      val initialSet = Factory.createVector(list)

      // Create an instance of the subset generator
      val gen = Factory.createSubSetGenerator(initialSet, false)

      val genSet = (for (subList <- gen) yield subList.getVector.toList).toSet

      assert( genSet.size == enum.toSet.size )
      val booleans = for (ind <- 0 until enum.size)
        yield genSet contains (enum(ind))

      booleans forall identity

    }

}