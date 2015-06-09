package scife.enumeration
package parallel
package memoization
package scope

import scife.enumeration.memoization._

import java.util.concurrent.LinkedBlockingQueue
import scala.collection.JavaConversions._

class AccumulatingConcurrentScope extends MemoizationScope {

  val memoizations = new LinkedBlockingQueue[Memoizable]()

  def add(m: Memoizable) = memoizations add m

  def clear = for (m <- memoizations) m.clearMemoization

}
