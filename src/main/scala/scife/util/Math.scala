package scife.util

object Math {

  def cantorInverse(z: Int) = {
    val t: Int = math.floor( (-1d + math.sqrt(1d + 8 * z))/ 2d ).toInt
    val y = z - t * (t + 1) / 2
    val x = t - y
    assert(t * (t + 3) / 2 - z == x)
//    fine("For z=%d, we got reverse (%d, %d)".format(z, x, y))
    (x, y)
  }

  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2

}
