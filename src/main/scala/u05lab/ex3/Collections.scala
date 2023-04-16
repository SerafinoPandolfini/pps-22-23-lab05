package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def testCollections(): Unit =

  val vector: Vector[Int] = Vector(1, 2, 3, 4)
  println("Test Vector: " + vector)
  println("Value in pos 0: " + vector(0))
  println("changed vector(2) = 30: " + vector.updated(2, 30))
  println("removed odd elements: " + vector.filter(_ % 2 == 0) + "\n")

  val array: Array[Int] = Array(1, 2, 3, 4)
  println("Test Array: " + array.mkString("Array(", ", ", ")"))
  println("Value in pos 0: " + array(0))
  println("changed array(2) = 30: " + array.updated(2, 30).mkString("Array(", ", ", ")"))
  println("removed odd elements: " + array.filter(_ % 2 == 0).mkString("Array(", ", ", ")") + "\n")

  val set: Set[Int] = Set(1, 2, 3, 4)
  println("Test Set: " + set)
  println("Value in pos 0: " + set(0))
  println("set with all values incremented by 1: " + set.map(i => i + 1))
  println("removed odd elements: " + set.filter(_ % 2 == 0) + "\n")

  val m: Map[Int, Int] = Map(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40)
  println("Test Map: " + m)
  println("Value of key 1 = " + m(1))
  println("map with all values incremented by the key: " + m.map((i1, i2) => (i1, i1 + i2)))
  println("removed element with key 1: " + m.removed(1) + "\n")

  val arrayBuffer: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer[Int](1, 2, 3 ,4)
  println("Test mutable ArrayBuffer: " + arrayBuffer)
  println("Value in pos 0: " + arrayBuffer(0))
  println("add 5 to arrayBuffer: " +   arrayBuffer.addOne(5))
  println("removed element in position 4: " + arrayBuffer.remove(4))
  println("updated arrayBuffer changing element in pos 1 with 20: " + arrayBuffer.updated(1, 20) + "\n")

  val mutableSet: mutable.Set[Int] = mutable.Set(1, 2, 3, 4)
  println("Test mutable Set: " + mutableSet)
  println("Size of the Set: " + mutableSet.size)
  println("Check if the set contans element 1: " + mutableSet(1))
  println("removed element 1 = " + (mutableSet -= 1) + "\n")

  println("\nMutable map")
  val mutableMap: mutable.Map[Int, Int] = mutable.Map[Int, Int](1 -> 10, 2 -> 20 ,3 -> 30, 4 -> 40)
  println("Test mutable Map: " + mutableMap)
  println("Size of the Map: " + mutableMap.size)
  println("keySet of the Map: " + mutableMap.keySet)
  println("values of the Map: " + mutableMap.values)
  println("get value of key 4: " + mutableMap.get(4) + "map now is: " + mutableMap)
  println("removed key 2: " + mutableMap.remove(2) + "map now is: " + mutableMap + "\n")

@main def checkPerformance(): Unit =

  val numberOfElemets = 100000

  /* immutable Linear sequences: List, mutable ListBuffer */
  val list: List[Int] = List.range(1, numberOfElemets)
  val listBuffer: ListBuffer[Int] = ListBuffer[Int]()
  listBuffer.addAll(1 to numberOfElemets)

  /* Indexed sequences: immutable Vector, Array, mutable ArrayBuffer */
  val vector: Vector[Int] = Vector.range(1, numberOfElemets)
  val array: Array[Int] = Array.range(1, numberOfElemets)
  val arrayBuffer: ArrayBuffer[Int] = ArrayBuffer[Int]()
  arrayBuffer.addAll(1 to numberOfElemets)

  /* Sets */
  val set: Set[Int] = Set.range(1, numberOfElemets)
  val mutableSet: mutable.Set[Int] = mutable.Set[Int]()
  mutableSet.addAll(1 to numberOfElemets)

  /* Comparison */
  import PerformanceUtils.*
  measure("list last")(list.last)
  measure("list buffer last")(listBuffer.last)
  measure("vector last")(vector.last)
  measure("array last")(array.last)
  measure("array buffer last")(arrayBuffer.last)
  measure("set last")(set.last)
  measure("mutable set last")(mutableSet.last)

