package insynth
package attrgrammar

import scala.collection.mutable.{ Map => MutableMap }

import org.kiama.attribution.Attribution
import org.kiama.attribution.Attributable
import org.kiama.attribution.Attribution._
import org.kiama.attribution.Decorators._

import streams._
import reconstruction.stream._
import util.logging._

import StreamableAST._

import scala.language.postfixOps

trait Streamables[T] {
  
    // streamables that can be attached with recursive edges
    type LazyStreamable = Streamable[T] with AddStreamable[T]

    val stream : StreamEl => Streamable[T]
      
    val listStream : ListStreamEl => Streamable[List[T]]
    
    val visited : StreamEl => Set[StreamEl]
    
    val cachedStreams: StreamEl => Map[StreamEl, Streamable[T]]
    
//    val recursiveParamsMap : StreamEl => Map[StreamEl, (LazyStreamable, Set[T])]
    
}

class StreamablesImpl[T](streamBuilder: StreamFactory[T]) extends Streamables[T]
  with HasLogger {

  import streamBuilder._
  
  type InjectionStreamValue = (T, Int)
        
  def getStreamPairs(streamEl: StreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[InjectionStreamValue], Boolean)],
    streamSpecificInjections: Map[StreamEl, (Stream[InjectionStreamValue], Boolean)] = Map(),
    filters: Map[Filter, T => Boolean] = Map()
  ) = {
    entering("getStreamPairs with injections: " + injections.map(_._2._1.mkString(",")))
    extractPairStream(
      getStreamable(streamEl, process, combiner, injections, streamSpecificInjections,
        filters)
    )
  }
  
  def getStreamListPairs(streamEl: ListStreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[InjectionStreamValue], Boolean)],
    streamSpecificInjections: Map[StreamEl, (Stream[InjectionStreamValue], Boolean)] = Map(),
    filters: Map[Filter, T => Boolean] = Map()
  ) = {
    extractPairStream(
      getStreamableList(streamEl, process, combiner, injections, streamSpecificInjections,
        filters)
    )
  }
    
  def getStream(streamEl: StreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[InjectionStreamValue], Boolean)],
    streamSpecificInjections: Map[StreamEl, (Stream[InjectionStreamValue], Boolean)] = Map(),
    filters: Map[Filter, T => Boolean] = Map()
  ) = {
    getStreamable(streamEl, process, combiner, injections, streamSpecificInjections,
      filters).getStream
  }  
  
  def getStreamable(streamEl: StreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[InjectionStreamValue], Boolean)],
    streamSpecificInjections: Map[StreamEl, (Stream[InjectionStreamValue], Boolean)] = Map(),
    filters: Map[Filter, T => Boolean] = Map()
  ) = {
    Attribution.resetMemo
    Attribution.initTree(streamEl)
          
    initialize
    this.combiner = combiner
    this.process = process
    this.injections = injections
    this.specificInjections = streamSpecificInjections
    this.filters = filters
    
    val transformed = stream(streamEl)
    
    postProcess
    
    transformed
  }
  
  def getStreamableList(streamEl: ListStreamEl,
    process: PartialFunction[(Class[_], T), T],
    combiner: PartialFunction[(Class[_], List[T]), T],
    injections: Map[Class[_], (Stream[InjectionStreamValue], Boolean)],
    streamSpecificInjections: Map[StreamEl, (Stream[InjectionStreamValue], Boolean)] = Map(),
    filters: Map[Filter, T => Boolean] = Map()
  ) = {
    Attribution.initTree(streamEl)
          
    initialize
    this.combiner = combiner
    this.process = process
    this.injections = injections
    this.specificInjections = streamSpecificInjections
    this.filters = filters
    
    val transformed = listStream(streamEl)
    
    postProcess
    
    transformed
  }
  
  val identityWithClass = (c: Class[_], t: T) => t

  val stream : StreamEl => Streamable[T] =
    dynAttr {
      case Single(c, inner) =>
        // TODO optimize this!, dont do unary stream if not necessary
        val modify: T => T = (t: T) => 
          if (process.isDefinedAt((c, t)))
            process(c, t)
          else
            t
        
        makeUnaryStream( inner->stream, modify ) 

      case i: Injecter =>
        val (innerStream, isInfinite) = 
          specificInjections.getOrElse(i, injections(i.c))
        
        fine("isInfinite " + isInfinite + " for " + i)
          
        if (isInfinite) makeSingleStream( innerStream )
        else makeFiniteStream( innerStream.toVector )
        
      case f@Filter(c, inner, _) =>
        if (filters.contains(f))
          makeFilterStream( inner->stream, filters(f) )
        else
          inner->stream        

      case a@Alternater(c, inner) => 
        // get node sets for both recursive and non-recursive edges
        val (recursiveParams, nonRecursiveParams) =
//          inner partition { a->visited contains _ }
          (a.getRecursiveLinks, inner)

        info("recursiveParams.size=" + recursiveParams.size +
          "nonRecursiveParams.size=" + nonRecursiveParams.size)
        
        // transform only non-recursive 
        val nonRecursiveStreamableList =
          nonRecursiveParams map { _->stream }

        // if there are recursive links we need to construct lazy stream
        val thisStream =
          if (recursiveParams.isEmpty)
            makeRoundRobbin(nonRecursiveStreamableList)
          else {
            val paramInitStream = makeLazyRoundRobbin(nonRecursiveStreamableList.toList)
            
            val kv: (StreamEl, (LazyStreamable, List[StreamEl])) =
              (a, (paramInitStream, recursiveParams.toList))
              
            recursiveParamsMap += kv
            
            paramInitStream
          }
        
        // TODO optimize this!, dont do unary stream if not necessary
        val modify: T => T = (t: T) =>
          if (process.isDefinedAt((c, t)))
            process(c, t)
          else
            t
            
        makeUnaryStream( thisStream, modify ) 

      case Combiner(c, inner) =>          
        // make streams of lists of parameter combinations
        val paramListStream = inner->listStream
        
        makeUnaryStream(paramListStream,
          (list: List[T]) => combiner(c, list)
          , Some(_ + 1))
        
      case Empty =>
        makeEmptyStreamable
    }
  
    val cachedStreams: StreamEl => Map[StreamEl, Streamable[T]] =
      attr {
        case t if t isRoot => Map( (t, (t->stream)) )
        case t             => {
          val kv = (t, (t->stream)): (StreamEl, Streamable[T])

          t.parent[StreamEl]->cachedStreams + kv
        }
      }
  
  val visited : Attributable => Set[StreamEl] =
    down[Attributable, Set[StreamEl]] {
        case t: StreamEl if t isRoot => Set(t)
        case t: StreamEl             => Set(t) | t.parent[Attributable]->visited
    }
  
  val listStream : ListStreamEl => Streamable[List[T]] =
    attr {
      case agg@Aggregator(inner) =>
//          val singletonList = makeUnaryStreamList(inner.head->stream, { t: T => List(t) })
        
        val paramsStreams: Seq[Streamable[T]] =
          inner map { _->stream }
        
        // make streams of lists of parameter combinations
        val paramListStream: Streamable[List[T]] =
          paramsStreams match {
            case Nil => makeSingletonList(Nil)
            case List(stream) => makeUnaryStreamList(stream, { el: T => List(el) })
            case stream1 :: stream2 :: rest =>
              ((makeBinaryStream(stream1, stream2) {
                (el1, el2) => List(el1, el2) }: Streamable[List[T]]) /: rest) {
                  (resStream: Streamable[List[T]], stream3: Streamable[T]) =>
                    makeBinaryStream(resStream, stream3) { (list, el2) => list :+ el2 }
                }
          }

        paramListStream
        
      case Generator(inner) =>
        val nilStream = streamBuilder.makeSingletonList(Nil)
        val genStream = inner->stream
        
        val listStream = streamBuilder.makeLazyRoundRobbinList(List(nilStream))

        val constructorStream =
          makeBinaryStream(listStream, genStream) { (list, el2) => list :+ el2 }
        
        listStream addStreamable constructorStream
        listStream.initialize

        listStream
    }
      
  var recursiveParamsMap: MutableMap[StreamEl, (LazyStreamable, List[StreamEl])] = _
//  var nodeMap: MutableMap[StreamEl, Streamable[T]] = _
  
  var combiner: PartialFunction[(Class[_], List[T]), T] = _
  var process: PartialFunction[(Class[_], T), T] = _
  var injections: Map[Class[_], (Stream[InjectionStreamValue], Boolean)] = _
  var specificInjections: Map[StreamEl, (Stream[InjectionStreamValue], Boolean)] = _
  var filters: Map[Filter, T => Boolean] = _
    
  // initialize data for each traversal
  def initialize = {
//    nodeMap = MutableMap.empty
    recursiveParamsMap = MutableMap.empty
  }
  
  // method that is called after the traversal to update recursive node children
  def postProcess = {
    for (
      (streamEl, (paramInitStream, recursiveLinks)) <- recursiveParamsMap
    ) {
      val recursiveStreams = recursiveLinks map ( _->stream )
      assert(recursiveStreams.size > 0, "recursiveStreams.size > 0")
    
      paramInitStream addStreamable recursiveStreams
      assert(!paramInitStream.isInitialized, "!paramInitStream.isInitialized")
      
      // initialize the lazy round robbin
      paramInitStream.initialize
    }
  }
  
  def extractPairStream(s: Streamable[_]) = 
    s match {
      case os: OrderedStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case us: Streamable[_] =>
        fine("returning unordered streamable")
        us.getStream zip Stream.continually(0f)
    }
  
//    val recursiveParamsMap : StreamEl => Map[StreamEl, (LazyStreamable, Set[T])] =
//      attr {
//        case Single(c, inner) => inner -> recursiveParamsMap
//        case a@Alternation(c, inner) => 
//        case CombinatorN(c, inner) =>          
//        case Empty => makeEmptyStreamable      
//    }

}