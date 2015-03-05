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

  object Binomial {
    
    import org.apache.commons.math3.util.CombinatoricsUtils.{ binomialCoefficient => bc }
        
    def main(args: Array[String]): Unit = {
      val n = 5
      val k = 3
      val result = binomialCoefficient(n, k)
      println("The Binomial Coefficient of %d and %d equals %d.".format(n, k, result))
    }

    def binomialCoefficient(n: Int, k: Int): Int = {
      require(n > 0 && k > 0, s"n=$n and k=$k")
      if (n <= 10 && k <= 10) cachedBinomials(n)(k) 
      else bc(n, k).toInt
//      ( (BigInt(n - k + 1) to n).product / (BigInt(1) to k).product).intValue
    }
      
    val cachedBinomials = {
      // 0 row/column not used
      val arr = Array.ofDim[Int](11, 11)
      for (i <- 1 to 10; j <- 1 to i)
        arr(i)(j) = ( bc(i, j).toInt )
      arr
    }
  }

  object Catalan {
    def factorial(n: BigInt) = BigInt(1).to(n).foldLeft(BigInt(1))(_ * _)

    def catalan(n: BigInt) = factorial(2 * n) / (factorial(n + 1) * factorial(n))
  }
  
}
