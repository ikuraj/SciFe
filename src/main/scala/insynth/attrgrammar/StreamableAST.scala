package insynth
package attrgrammar

import scala.collection._

import scala.language.existentials

/**
 * Language AST for streamables
 */
object StreamableAST {

    import org.kiama.attribution.Attributable
    import org.kiama.util.Positioned

    trait StreamEl extends Attributable with Positioned
    
    // TODO make it single el
    
    /** Allows injection of a stream of values at run-time */
    case class Injecter(c: Class[_], id: Int = 0) extends StreamEl

    /** Allows mapping of elements of inner stream */ 
    case class Single(c: Class[_], inner: StreamEl) extends StreamEl
    
    case class Combiner(c: Class[_], inner: ListStreamEl) extends StreamEl
    
    case class Alternater(c: Class[_], val inner: Seq[StreamEl]) extends StreamEl {
      
      var recList = List[StreamEl]()
      
      def addStreamEl(se: StreamEl) = recList :+= se
      
      def getRecursiveLinks = recList
        
    }

    case class Filter(c: Class[_], inner: StreamEl, id: Int = 0) extends StreamEl
    
    case object Empty extends StreamEl
    
//    case class Combinator2(c: Class[_], left: StreamEl, right: StreamEl) extends StreamEl
        
    abstract class ListStreamEl extends Attributable with Positioned
    // generates a list of elements from a single stream
    // NOTE this cannot be expressed in the typing system (something like a function with infinite number of
    // parameters of the same type)
    case class Aggregator(inner: Seq[StreamEl]) extends ListStreamEl
    case class Generator(inner: StreamEl) extends ListStreamEl
    // will not produce an empty list
//    case class AggregatorNoEmpty(c: Class[_], inner: StreamEl) extends ListStreamEl
    
    // TODO to this with class?
    // we want unique injecters
    object Injecter {
      var newId = 0
      
      def newInjecter(c: Class[_]) = {
        newId += 1
        new Injecter(c, newId)
      }
    }

    object Filter {
      var newId = 0
      
      def newFilter(c: Class[_], el: StreamEl) = {
        newId += 1
        new Filter(c, el, newId)
      }
    }

}
