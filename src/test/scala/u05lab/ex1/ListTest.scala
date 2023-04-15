package u05lab.ex1

import org.junit.Test
import org.junit.*
import org.junit.Assert.*

class ListTest:
  
  var reference: List[Int] = List()
  @Before
  def initialize(): Unit =
    reference = List(1, 2, 3, 4)
  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), reference.partition(_ % 2 == 0))

  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span(_ < 3)) 
    
  @Test
  def takeRight(): Unit =
    assertEquals(List(2, 3, 4), reference.takeRight(3))
  

