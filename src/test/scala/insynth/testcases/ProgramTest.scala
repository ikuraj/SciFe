package insynth
package testcases

import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet }
import org.kiama.attribution.Attribution

import insynth.structures._
import insynth.reconstruction.stream._
import insynth.reconstruction._
import insynth.attrgrammar._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers
import org.scalacheck._

import insynth.common._
import insynth.testdomain.{ TestQueryBuilder => QueryBuilder, _ }
import insynth.util._

class ProgramTest extends FunSuite {
  import StreamableAST._
  import Structures._
  
  case class Id(id: Int)
  
  trait Identifiable
 
  case class Program(classes: Seq[Class])
  case class Class(id: Id, methods: Seq[Method]) extends Identifiable
  case class Method(id: Id, statements: Seq[Statement]) extends Identifiable {
    require(
      (for (ind <- 0 until statements.size)
        yield usedVars(statements(ind)) subsetOf definedVars(statements.take(ind))
  		).reduce( _ && _ )
    )
    def usedVars(s: Statement) = Set[Id]()
    def definedVars(s: Seq[Statement]) = Set[Id]()
  }
  abstract class Statement
  case class MethodCall(callee: Id, argument: Seq[Expression])
  case class Assignment(varId: Id, exp: Expression) extends Statement with Identifiable
  abstract class Expression
  case class Var(id: Id) extends Expression
  case class IntExp(v: Int) extends Expression
  case class BooleanExp(v: Boolean) extends Expression
  
  var identifiers = Map[Int, Identifiable]()
  
  val integers = Injecter(classOf[IntExp])
  val booleans = Injecter(classOf[BooleanExp])
  val ids = Injecter(classOf[Id])
  val vars = Single(classOf[Var], ids)
  val expressions = Alternater(classOf[Expression], List(integers, booleans, vars))

  val assignmentAggr = Aggregator(Seq(ids, expressions))
  val assignments = Combiner(classOf[Assignment], assignmentAggr)

  val expressionLists = Generator(expressions)
  val methodCallAggr = Aggregator(Seq(ids, expressionLists))
  val methodCalls = Combiner(classOf[MethodCall], methodCallAggr)

  val statements = Alternater(classOf[Statement], List(methodCalls, assignments))

  val headElement = statements
  
  val streamFactory = new OrderedStreamFactory[Any]
  val streamables = new StreamablesImpl(streamFactory)

  val intStream = Stream( 1, 2, 3 ).map(new IntExp(_)) zip ones
  val booleansStream = Stream( false, true ).map(new BooleanExp(_)) zip ones
  val idStream = Stream.from(1).map(Id(_)) zip fromOne

  val resultStream = streamables.getStreamPairs(
    headElement,
    Map(
      vars ->  { (a: Any) => Var(a.asInstanceOf[Id])}
    ),
    {
      case (clazz, (id: Id) :: (e: Expression) :: Nil) if clazz == classOf[Assignment] =>
        new Assignment(id, e)
      case (clazz, (id: Id) :: list) if clazz == classOf[MethodCall] =>
        new MethodCall(id, list.asInstanceOf[List[Expression]])
      case (clazz, e) => throw new RuntimeException("case not found " + (clazz, e))
    },
    Map( classOf[IntExp] -> ( intStream, false ), classOf[BooleanExp] -> ( booleansStream, false ),
        classOf[Id] -> ( idStream, true )),
    Map(),
    Map()
  )
  
  info(
		resultStream.take(100).mkString("\n")
	)


}
