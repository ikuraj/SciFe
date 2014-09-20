//package scife.enumeration
//package reverse
//package dependent
//
//import scife.enumeration.{ combinators => ecomb }
//
//import scife.util.logging._
//
//object Product {
//
//  def apply[I, O1, O2](s1: ReverseDepend[I, O1], s2: ReverseDepend[I, O2]) = {
//    new Product(s1, s2) with ReverseDepend[I, (O1, O2)]
//  }
//
//}
