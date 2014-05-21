package insynth.load

import org.scalatest.junit.JUnitSuite
import org.junit.{ Test, Ignore }
import org.junit.Assert._

import insynth.testdomain._
import insynth.common._

// just to be sure JUnit behaves as expected
class DeclarationTestJUnit extends JUnitSuite {
  
  import CommonDeclarations._
  val dt = CommonDomainTypes
  val st = CommonSuccinctTypes
  
  @Test
  def declarationsThroughFactoryMethods {
    assertEquals(TestDeclaration(dt.typeInt), TestDeclaration(dt.typeInt))
    assertEquals(TestDeclaration(dt.typeInt, "name"), TestDeclaration(dt.typeInt, "name"))    
  }
  
}