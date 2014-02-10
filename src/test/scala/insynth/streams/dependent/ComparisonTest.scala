//package insynth
//package streams
//package dependent
//
//import org.scalatest.FunSuite
//import org.scalatest.matchers._
//
//import reconstruction.stream._
//import insynth.attrgrammar._
//import StreamableAST._
//
//import util._
//import util.logging._
//import util.format._
//import util.format._
//import common._
//
//class ComparisonTest extends FunSuite with ShouldMatchers with HasLogger with ProfileLogger {
//
//  test("generation of sorted lists with arbitrary values") {
//
//    val maxLength = 10
//    
//    {
//      val intProducer = Producer(
//        { (v: Int) => Stream.from(v, -1) })
//      val listChooser = Producer(
//        { (v: Int) => Stream.from(v).map(el => List(el to maxLength: _*)) })
//      val binaryStream = Binary(intProducer, listChooser)(_ + 1)(
//        (v: Int, l: List[Int]) => v :: l)
//
////      withClue(binaryStream.getStream(5).take(5).mkString(", ")) {
////        binaryStream.getStream(0) should be(Nil)
////        binaryStream.getStream(1) should be(Stream(List(1)))
////        binaryStream.getStream(2).map(_.reverse) should be(Stream(List(1, 2)))
////        binaryStream.getStream(3).map(_.reverse) should be(Stream(List(1, 2, 3)))
////      }
//
//      val taken = binaryStream.getStream(5).take(100)
//      println(taken)
//
//      assert(taken.forall(list => list.sorted == list))
//    }
//
//    {
//      // with lazy streams
//      val injectInt1 = Injecter(classOf[Int])
//      val injectInt2 = Injecter(classOf[Int])
//      val intNode = Alternater(classOf[Int], Seq(injectInt1, injectInt2))
//      val nilNode = Injecter(classOf[List[Int]])
//      val listNode = Alternater(classOf[List[Int]], List(nilNode))
//      val listParamNode = Aggregator(Seq(intNode, listNode))
//      val consNode = Combiner(classOf[List[Int]], listParamNode)
//      val filterListValNode = Filter(classOf[List[Int]], consNode)
////      val rootFilter = Filter(classOf[List[Int]], filterListValNode)
//      listNode.addStreamEl(filterListValNode)
//
//      val streamFactory = new OrderedStreamFactory[Any]
//
//      val streamables = new StreamablesImpl(streamFactory)
//
//      val intStreamPos = Stream.from(1) zip Stream.continually(1)
//      val intStreamNeg = Stream.from(0, -1) zip Stream.continually(1)
//      val intStream = Stream.from(0) zip Stream.continually(1)
//      val nilStream = Stream((Nil, 1))
//
//      val resultStream = streamables.getStreamPairs(
//        filterListValNode,
//        Map(),
//        {
//          case (clazz, (a: Int) :: (b: List[_]) :: Nil) if clazz == classOf[List[Int]] =>
//            a :: b
//        },
//        Map(),
//        Map(injectInt1 -> (intStreamPos, true), injectInt2 -> (intStreamNeg, true),
//          nilNode -> (nilStream, false)),
//        Map(filterListValNode -> ((e: Any) =>
//          e.asInstanceOf[List[Int]].sorted == e.asInstanceOf[List[Int]]
//          )
////          ,
////          rootFilter -> ((e: Any) =>
////          e.asInstanceOf[List[Int]].size == maxLength
//          )
//        )
//      )
//
//      val taken = resultStream.take(100).asInstanceOf[Stream[(List[Int], Int)]]
//      println(taken)
//
//      assert(taken.map(_._1).forall(list => list.sorted == list))
//      assert(taken.map(_._1).forall(list => list.size == maxLength))
//      //      assert( Checks.noRepeat(taken) )
//
//    }
//
//  }
//
//}