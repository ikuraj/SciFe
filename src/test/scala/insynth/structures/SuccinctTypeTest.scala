package insynth.structures

import org.scalatest.junit.JUnitSuite

import org.junit.Assert._
import org.junit.Test

class SuccinctTypeTest extends JUnitSuite {
  
  @Test
  def testTree1 {
    assertEquals (Const("A"),Const("A"))
  }
  
  @Test
  def testTree2 {
    assertEquals (TSet(List(Const("A"),Const("B"))),TSet(List(Const("A"),Const("B"))))
  }

  @Test
  def testTree3 {
    assert (TSet(List(Const("A"),Const("B"))) equals TSet(List(Const("A"),Const("B"))))
  }
  
  
  @Test
  def testTree4 {
    assert (TSet(List(Const("A"),Const("B"))) == TSet(List(Const("A"),Const("B"))))
  }
  
  @Test
  def testTree5 {
    assert (TSet(List.empty[SuccinctType]) == TSet(List.empty[SuccinctType]))
  }


  @Test
  def testTree6 {
    assert (TSet(List(Const("A"))) subsetOf TSet(List(Const("A"),Const("B"))))
  }
  
  @Test
  def testTree7 {
    assert (TSet(List()) subsetOf TSet(List(Const("A"),Const("B"))))
  }
  
  @Test
  def testTree8 {
    assert (TSet(List(Const("A"),Const("B"))) subsetOf TSet(List(Const("A"),Const("B"))))
  }
  
  @Test
  def testTree9 {
    assert (TSet(List(Const("B"), Const("A"))) subsetOf TSet(List(Const("A"),Const("B"))))
  }
  
}