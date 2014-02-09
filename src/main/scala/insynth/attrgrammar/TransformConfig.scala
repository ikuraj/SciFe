package insynth
package attrgrammar

import scala.collection.mutable.{ Map => MutableMap }
import streams._

import StreamableAST._
	
case class TransformConfig[T](
  process: PartialFunction[(Class[_], T), T],
  combiner: PartialFunction[(Class[_], List[T]), T],
  injections: Map[Class[_], (Stream[(T, Int)], Boolean)],
  streamSpecificInjections: Map[StreamEl, (Stream[(T, Int)], Boolean)] = Map(),
  filters: Map[Filter, T => Boolean] = Map()
)