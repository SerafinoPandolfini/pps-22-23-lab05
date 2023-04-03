package u05lab.ex1

import org.junit.Test
import org.junit.*
import org.junit.Assert.*

class ListTest {

  /** todo */
  @Test
  def testZipRight(): Unit =
    val reference = List(1, 2, 3, 4)
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRight)

}
