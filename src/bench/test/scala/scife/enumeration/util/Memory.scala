package scala.scife.enumeration.util

object Memory {

  def getFreeMemory = {
    import java.lang.management._

    val memoryBean = ManagementFactory.getMemoryMXBean();
    val heapUsage = memoryBean.getHeapMemoryUsage();
    val maxMemory = heapUsage.getMax() / (1024 * 1024);
    val usedMemory = heapUsage.getUsed() / (1024 * 1024);
    println(" : Memory Use :" + usedMemory + "M/" + maxMemory + "M");

    (maxMemory - usedMemory).toInt
  }

  def tryToFreeUpSpaceG(toHaveFree: Int) = {
    System.gc; System.gc
    // try to force GC
    var currentFree = getFreeMemory
    while (currentFree < toHaveFree) {
      currentFree = getFreeMemory
      try {
        @transient
        var bigBuffer = Array.ofDim[Byte](toHaveFree, 1024 * 1024)
        System.gc; System.gc
        bigBuffer = null
        System.gc; System.gc
      } catch {
        case e: OutOfMemoryError =>
          System.gc; System.gc
          Thread.sleep(1000)
          System.gc; System.gc
        case e: Exception =>
          e.printStackTrace();
      }
    }
  }

}