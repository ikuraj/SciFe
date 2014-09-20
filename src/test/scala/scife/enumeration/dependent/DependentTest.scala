package scife
package enumeration
package dependent

import org.scalatest._

class DependentTest extends FunSuite with Matchers {

  test("factory method") {

    {
      val enum = Enum(1, 2, 3)

      enum shouldBe a [WrapArray[_]]
      enum.hasDefiniteSize should be (true)
      enum.size should be (3)

      val f = { (_: Int) => enum }

      val tdenum = Depend( { (self: Depend[Int, Int], i: Int) => f(i) } )

      val gotEnum: Finite[Int] = tdenum(1)
      gotEnum shouldBe a [Finite[_]]

      val gotEnum2: Finite[Int] = tdenum(1)
    }

  }

}
