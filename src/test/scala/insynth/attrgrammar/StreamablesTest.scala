package insynth
package attrgrammar

import org.scalatest._
import org.scalatest.matchers._

import insynth.reconstruction.stream.Application
import insynth.common._
import insynth.reconstruction.stream._
import insynth.streams.{ ordered => ord }

import util._
import util.format._

class StreamablesTest extends FunSuite with ShouldMatchers {

  import StreamableAST._
  import CommonLambda._
  import Structures._
  
  case object Type
  
  case class Program(classes: Seq[Class])
  case class Class(methods: Seq[Method])
  case class Method(info: Int)
//  case class Method(statements: Seq[Statement])
  
  val streamFactory = new OrderedStreamFactory[Any]
  
//  test("simpleConstruction") {
//    
//    case class Program(classes: Class)
//    case class Class(methods: Method)
//    case class Method()
//    
//    val methodNode = Empty
//    val classNode = Single(classOf[Class], methodNode)
//    val programNode = Single(classOf[Program], classNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val resultStream = streamables.getStreamable(
//      programNode,
//      {
//        case (clazz, c: Class) if clazz == classOf[Program] => Program(c)
//        case (clazz, m: Method) if clazz == classOf[Class] => Class(m)
//      },
//      null,
//      null
//    )
//
//    resultStream match {
//      case us: ord.UnaryStream[_, _] =>
//        us.streamable match {
//          case us: ord.UnaryStream[_, _] =>
//            us.streamable match {
//              case ord.Empty =>                
//              case _ => fail          
//            }            
//          case _ => fail          
//        }
//      case _ => fail
//    }
//      
//  }
//
//  test("injecter stream simple test") {
//    
//    case class Program(classes: Class)
//    case class Class(methods: Method)
//    case class Method(m: Int)
//    
//    val methodNode = Injecter(classOf[Method])
//    val classNode = Single(classOf[Class], methodNode)
//    val programNode = Single(classOf[Program], classNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val methodStream = Stream( Method(1), Method(2), Method(3) )
//    
//    val resultStream = streamables.getStreamable(
//      programNode,
//      {
//        case (clazz, c: Class) if clazz == classOf[Program] => Program(c)
//        case (clazz, m: Method) if clazz == classOf[Class] => Class(m)
//      },
//      null,
//      Map( classOf[Method] -> ( methodStream, false ) )
//    )
//
//    resultStream match {
//      case us: ord.UnaryStream[_, _] =>
//        us.streamable match {
//          case us: ord.UnaryStream[_, _] =>
//            us.streamable match {
//              case ss: ord.FiniteStream[_] => 
//                assert( ss.getStream.toList == methodStream.toList) 
//              case _ => fail          
//            }            
//          case _ => fail          
//        }
//      case _ => fail
//    }
//    
//    expectResult( methodStream.map(m => Program(Class(m))) ) {
//      resultStream.getStream
//    }    
//      
//  }
//  
//  test("aggregator stream simple test") {
//    
//    case class Program(classes: Class)
//    case class Class(methods: Seq[Method])
//    case class Method(m: Int)
//    
//    val methodNode = Injecter(classOf[Method])
//    val aggregatedMethodNode = Generator(classOf[Method], methodNode)
//    val classNode = Combiner(classOf[Class], aggregatedMethodNode)
//    val programNode = Single(classOf[Program], classNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val methodStream = Stream( Method(1), Method(2), Method(3) )
//    
//    val resultStream = streamables.getStreamPairs(
//      programNode,
//      {
//        case (clazz, m: Class) if clazz == classOf[Program] => Program(m)
//      },
//      {
//        case (clazz, methodList: List[_]) if clazz == classOf[Class] =>
//          Class(methodList.asInstanceOf[List[Method]])
//      },
//      Map( classOf[Method] -> ( methodStream, false ) )
//    )
//    
//    val expected =
//      List((Nil, 1.0), (List(Method(1)), 2.0), (List(Method(2)), 3.0), (List(Method(3)), 4.0),
//        (List(Method(1), Method(1)), 3.0), (List(Method(1), Method(2)), 4.0)) map {
//        case (list, w) => (Program(Class(list)), w + 1)
//      }
//        
//    withClue ( resultStream.take(25).mkString("\n") ) {
//      for(ex <- expected)
//        assert( resultStream.take(25).toSet contains ex, "does not contain " + ex )
//    }    
//    
//  }
//  
//  test("alternater stream simple test - streamable object") {
//    
//    case class Method(classes: Expression)
//    case class Expression(a: Int)
//    
//    val intValNode = Injecter(classOf[Int])
//    val intValNode2 = Injecter(classOf[Int])
//    val exprNode = Alternater(classOf[Expression], Seq(intValNode, intValNode2))
//    val methodNode = Single(classOf[Method], exprNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val resultStream = streamables.getStreamable(
//      methodNode,
//      {
//        case (clazz, e: Expression) if clazz == classOf[Method] => Method(e)
//      },
//      null,
//      null,
//      Map( intValNode -> ( Stream(1, 2, 3), false ), intValNode2 -> ( Stream(3, 7, 8), false ) )
//    )
//    
//    withClue(FormatStreamUtils(resultStream)) {
//      resultStream match {
//        case us: ord.UnaryStream[_, _] =>
//          us.streamable match {
//            case us: ord.UnaryStream[_, _] =>
//              us.streamable match {
//                case us: ord.RoundRobbin[_] =>
//                case _ => fail          
//              }
//          }
//        case _ => fail
//      }
//    }
//        
//  }
//  
//  test("alternater stream simple test") {
//    
//    case class Method(classes: Expression)
//    case class Expression(a: Int)
//    
//    val intValNode = Injecter.newInjecter(classOf[Int])
//    val intValNode2 = Injecter.newInjecter(classOf[Int])
//    val exprNode = Alternater(classOf[Expression], Seq(intValNode, intValNode2))
//    val methodNode = Single(classOf[Method], exprNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val resultStream = streamables.getStreamPairs(
//      methodNode,
//      {
//        case (clazz, e: Expression) if clazz == classOf[Method] => Method(e)
//        case (clazz, e: Int) if clazz == classOf[Expression] => Expression(e)
//      },
//      null,
//      null,
//      Map( intValNode -> ( Stream(1, 2, 3), false ), intValNode2 -> ( Stream(3, 7, 8), false ) )
//    )
//    
//    val expected =
//      List(1, 2, 3, 7, 8) map {
//        case num => Method(Expression(num))
//      }
//
//    resultStream.take(25).size should be (6)
//    withClue ( resultStream.take(25).mkString("\n") ) {
//      for (ex <- expected)
//        assert( resultStream.take(25).map(_._1) contains ex, "does not contain " + ex )
//    }    
//    
//  }
//  
//  test("alternater stream test") {
//    
//    case class Method(exp: Expression)
//    trait Expression
//    case class Add(a: Int, b: Int) extends Expression
//    case class Multiply(a: Int, b: Int) extends Expression
//    
//    val intValNode = Injecter(classOf[Int])
//    val intValuesNode = Aggregator(classOf[Int], Seq(intValNode, intValNode))
//    val addNode = Combiner(classOf[Add], intValuesNode)
//    val mulNode = Combiner(classOf[Multiply], intValuesNode)
//    val exprNode = Alternater(classOf[Expression], Seq(addNode, mulNode))
////    val exprNode = Alternater(classOf[Expression], Seq(addNode))
//    val methodNode = Single(classOf[Method], exprNode)
//    
//    val streamables = new StreamablesIml(streamFactory)
//    
//    val resultStream = streamables.getStreamPairs(
//      methodNode,
//      {
//        case (clazz, e: Expression) if clazz == classOf[Method] => Method(e)
//      },
//      {
//        case (clazz, (a: Int) :: (b: Int) :: Nil) if clazz == classOf[Multiply] =>
//          Multiply(a, b)
//        case (clazz, (a: Int) :: (b: Int) :: Nil) if clazz == classOf[Add] =>
//          Add(a, b)
//      },
//      Map( classOf[Int] -> ( Stream(1, 2, 3), false ) )
//    )
//    
//    val expected: List[Expression] =
//      generateLists(2, 1 to 3) flatMap {
//        case a :: b :: Nil =>
//          Add(a, b) :: Multiply(a, b) :: Nil
//        case _ => fail
//      }
//        
//    withClue ( resultStream.take(25).mkString("\n") ) {
//      resultStream.take(25).size should be (3 * 3 * 2)
//      for(ex <- expected)
//        assert( resultStream.take(25).map(_._1).toSet contains Method(ex), "does not contain " + ex )
//    }    
//    
//  }
  
  test("simple alternater streamable recursion test") {
    
    trait Expression
    case class Add(a: Expression, b: Expression) extends Expression    
    case class JustInt(a: Int) extends Expression
    
    val intValNode = Injecter(classOf[Int])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val exprNode = Alternater(classOf[Expression], List(justIntNode))
    val exprsNode = Aggregator(Seq(exprNode, exprNode))
    val addNode = Combiner(classOf[Add], exprsNode)
    exprNode.addStreamEl(addNode)
    
    val streamables = new StreamablesIml(streamFactory)
    
    val resultStream = streamables.getStreamable(
      addNode,
      {
        case (clazz, e: Int) if clazz == classOf[JustInt] => JustInt(e)
      },
      {
        case (clazz, (a: Expression) :: (b: Expression) :: Nil) if clazz == classOf[Add] =>
          Add(a, b)
      },
      Map( classOf[Int] -> ( Stream(1, 2, 3), false ) )
    )
    
    withClue(FormatStreamUtils(resultStream)) {
      resultStream match {
        case us: ord.UnaryStream[_, _] =>
          us.streamable match {
            case bs: ord.BinaryStream[_, _, _] =>
              bs.s2 match {
                case us: ord.UnaryStream[_, _] =>
                  us.streamable match {
                    case bs: ord.LazyRoundRobbin[_] =>
                      
                    case _ => fail
                  }                  
                case _ => fail          
              }
          }
        case _ => fail
      }
    }
    
  }
  
  test("simple alternater stream recursion test") {
    
    trait Expression
    case class Add(a: Expression, b: Expression) extends Expression    
    case class JustInt(a: Int) extends Expression
    
    val intValNode = Injecter(classOf[Int])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val exprNode = Alternater(classOf[Expression], List(justIntNode))
    val exprsNode = Aggregator(Seq(exprNode, exprNode))
    val addNode = Combiner(classOf[Add], exprsNode)
    exprNode.addStreamEl(addNode)
    
    val streamables = new StreamablesIml(streamFactory)
    
    val resultStream = streamables.getStreamPairs(
      addNode,
      {
        case (clazz, e: Int) if clazz == classOf[JustInt] => JustInt(e)
      },
      {
        case (clazz, (a: Expression) :: (b: Expression) :: Nil) if clazz == classOf[Add] =>
          Add(a, b)
      },
      Map( classOf[Int] -> ( Stream(1, 2, 3), false ) )
    )
    
    withClue ( resultStream.take(50).mkString("\n") ) {
      resultStream.take(50).size should be (50)
      for(ex <- List(
        (Add(Add(JustInt(1), JustInt(1)), Add(JustInt(2), JustInt(1))), 8.0)
      ))
        assert( resultStream.take(50).toSet contains ex, "does not contain " + ex )
    }    
    
  }

  test("alternater stream recursion test") {
    
    trait Expression
    case class Add(a: Expression, b: Expression) extends Expression    
    case class Mul(a: Expression, b: Expression) extends Expression    
    case class JustInt(a: Int) extends Expression
    
    val intValNode = Injecter(classOf[Int])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val exprNode = Alternater(classOf[Expression], List(justIntNode))
    val exprsNode = Aggregator(Seq(exprNode, exprNode))
    val addNode = Combiner(classOf[Add], exprsNode)
    val mulNode = Combiner(classOf[Mul], exprsNode)
    exprNode.addStreamEl(addNode)
    exprNode.addStreamEl(mulNode)
    
    val streamables = new StreamablesIml(streamFactory)
    
    val resultStream = streamables.getStreamPairs(
      addNode,
      {
        case (clazz, e: Int) if clazz == classOf[JustInt] => JustInt(e)
      },
      {
        case (clazz, (a: Expression) :: (b: Expression) :: Nil) if clazz == classOf[Add] =>
          Add(a, b)
        case (clazz, (a: Expression) :: (b: Expression) :: Nil) if clazz == classOf[Mul] =>
          Mul(a, b)
      },
      Map( classOf[Int] -> ( Stream(1, 2, 3), false ) )
    )
    
    withClue ( resultStream.take(100).mkString("\n") ) {
      resultStream.take(100).size should be (100)
      for(ex <- List(
        (Add(Mul(JustInt(1), JustInt(1)), Add(JustInt(2), JustInt(1))), 8.0)
      ))
        assert( resultStream.take(100).toSet contains ex, "does not contain " + ex )
    }    
    
  }
  
//  @Test
//  def simpleConstruction {
//    
//    
//    val intNode = Injectable(classOf[Int])
//    val methodNode = Single(classOf[Method], intNode)
//    val methodCombiner = Single(classOf)
//    val classNode = Single(classOf[Class], methodNode)
//    val programNode = Single(classOf[Program], classNode)
//      
//  }

}
