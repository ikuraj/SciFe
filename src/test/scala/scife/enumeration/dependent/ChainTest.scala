package scife.enumeration
package dependent

import _root_.scife.{ enumeration => e }
import org.scalatest._

import util._

class ChainTest extends FunSuite with Matchers {

  test("empty binary") {

    val emptyProducer = Depend(
      { (v: Int) => e.Empty }
    )

    val array = e.WrapArray( 1 to 100 )


    val binary = Chain(array, emptyProducer)

    binary.size should be (0)

  }

}
