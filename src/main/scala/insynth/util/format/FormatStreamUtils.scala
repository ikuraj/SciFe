package insynth.util.format

import scala.text._

import insynth.streams.Streamable
import insynth.streams.unordered._
import insynth.streams.ordered 

import insynth.util.format._
import insynth.util.format.FormatHelpers._

/**
 * how are intermediate nodes formated into pretty print documents
 */
object FormatStreamUtils {
  def apply[_](node: Streamable[_]) = new FormatStreamUtils(node, -1)
  def apply[_](node: Streamable[_], level: Int) = new FormatStreamUtils(node, level: Int)
}

class FormatStreamUtils[_](node: Streamable[_], _level: Int) extends Formatable {
  
  import FormatHelpers._
  
  override def toDocument = toDocument(node, _level)
  
  def toDocument(node: Streamable[_], level: Int): Document = {
    trans(node, level, Set())
  }
  
  def printStreamsWithIndexForRoundRobbin(rr: RoundRobbin[_]) = nestedBrackets(
    ((DocNil: Document) /: (rr.streams map { s => strToDoc(rr.toString) } zip (0 to rr.streams.size))) {
    	(res, pair) => res :/: "," :/: paren(pair._1 :: "-" :: pair._2.toString)
    }
  )
  
  def trans(node: Streamable[_], level: Int, visited: Set[Streamable[_]]): Document = {
    
    def header(node: Streamable[_]) = {
      sqBrackets(
	      {
	        node match {
	          case _: ordered.Memoized[_] | _: Memoized[_] => "M-": Document
	          case _ =>
	            DocNil
	        }
	      } :: node.##
      )
      //+ "[Inf?" + node.isInfinite + "]")
    }
    
    if (level == 0)
      return DocNil
      
    if (visited contains node)
    	return "Recursion@" :: header(node)
    
    val resDocument: Document = node match {
      
      case ordered.Empty => "ord.Empty"
      case Empty => "Empty"

      case s: ordered.Singleton[_] => "ord.Singleton" :: header(node) :: brackets {
        s.getValuedStream.head.toString
      }
      case s: Singleton[_] => "Singleton" :: header(node) :: brackets {
        s.getStream.head.toString
      }

      case ss: SingleStream[_, _] => "SingleStream" :: header(node)// :: paren(trans(ss.stream, level - 1))
      case ss: ordered.WrapperStream[_] => "ord.SingleStream" :: header(node)// :: paren(trans(ss.stream, level - 1))
      case fs: ordered.FiniteStream[_] => "ord.FiniteStream" :: header(node) :: brackets(
        foldDoc(
          fs.getValuedStream.map(_.toString: Document).take(3).toList :::
          { if (fs.getValuedStream.size > 3) "...": Document else DocNil } :: Nil,
          ", "
        )
      )

      case us: ordered.UnaryStream[_, _] => "ord.UnaryStream" :: header(node) ::
      	paren(trans(us.streamable, level - 1, visited + node))
      case us: ordered.UnaryStreamWithValueMod[_, _] => "ord.UnaryStreamWithValueMod" :: header(node) ::
      	paren(trans(us.streamable, level - 1, visited + node))
      case us: UnaryStream[_, _] => "UnaryStream" :: header(node) ::
      	paren(trans(us.streamable, level - 1, visited + node))
      	      
      case obs: ordered.BinaryStream[_, _, _] => "ord.BinaryStream" :: header(node) :: nestedBrackets(
        trans(obs.s1, level -1, visited + node) :/: trans(obs.s2, level -1, visited + node)
      )
      case bs: BinaryStream[_, _, _] => "BinaryStream" :: header(node) :: nestedBrackets(
        trans(bs.s1, level -1, visited + node) :/: trans(bs.s2, level -1, visited + node)
      )

      case lrr: LazyRoundRobbin[_] => "LazyRoundRobbin" :: header(node) :/: nestedBrackets(
        foldDoc(lrr.getStreamables map { trans(_, level - 1, visited + node) }, "\n")
      )
      case lrr: ordered.LazyRoundRobbin[_] => "ord.LazyRoundRobbin" :: header(node) :: nestedBrackets(
        foldDoc(lrr.getStreamables map { trans(_, level - 1, visited + node) }, "\n")
      )

      case rr: ordered.RoundRobbin[_] => "ord.RoundRobin" :: header(node) :/: nestedBrackets(
        ((DocNil: Document) /: rr.streams) { (res, doc) => res :: trans(doc, level - 1, visited + node) }
      )
      case rr: RoundRobbin[_] => "RoundRobin" :: header(node) :/: nestedBrackets(
        ((DocNil: Document) /: rr.streams) { (res, doc) => res :: trans(doc, level - 1, visited + node) }
      )

      case fs: ordered.FilterStream[_] => "ord.FilterStream" :: header(node)  :/:
        paren(trans(fs.streamable, level - 1, visited + node))
      case fs: ordered.FilterStreamCounted[_] => "ord.FilterStreamCounted" :: header(node)  :/:
        paren(trans(fs.streamable, level - 1, visited + node))
        
      case _ => "Dont know (%s): ".format(node.getClass.toString) :: header(node)
    }
    
    resDocument// :: brackets("Inf?" + node.isInfinite)
  }
}
