import scala.io.StdIn
import scala.util.Random

trait SortingWork {

  private val rand = new Random()

  import Ordering.Implicits._
  def partition[T](array: Array[T], l: Int, r: Int)(implicit numeric: Numeric[T]): (Int, Int) = {
    val x = array(l)
    var m1 = l
    var m2 = l
    for (i <- l + 1 to r) {
      val y = array(i)
      if (y <= x) {
        // first swap  i with m2+1
        val t = y
        array(i) = array(m2 + 1)
        array(m2 + 1) = t
      }
      if (y == x) {
        // then move m2 one place
        m2 += 1
      } else if (y < x) {
        // then swap m2+1 with m1
        val t = array(m1)
        array(m1) = array(m2 + 1)
        array(m2 + 1) = t

        // move m1& m2 one place
        m1 += 1
        m2 += 1
      } else {
        // we don't need to do any thing
      }
    }
    (m1, m2)
  }

  private def randomizedQuickSort[T](array: Array[T], l: Int, r: Int)(implicit numeric: Numeric[T]): Unit =
    if (l < r) {
      val pivot = rand.nextInt(r - l) + l

      val t = array(l)
      array(l) = array(pivot)
      array(pivot) = t

      val (m1, m2) = partition(array, l, r)
      randomizedQuickSort(array, l, m1 - 1)
      randomizedQuickSort(array, m2 + 1, r)
    }

  def randomizedQuickSort[T](array: Array[T])(implicit numeric: Numeric[T]): Unit =
    randomizedQuickSort(array, 0, array.length - 1)

}

object Sorting extends App with SortingWork {
  private val array = readData
  randomizedQuickSort(array)
  println(array.mkString(" "))

  private def readData: Array[BigInt] = {
    val n = StdIn.readInt()
    val array = Array.ofDim[BigInt](n)
    val line = StdIn.readLine()
    val data = line.split(" ")
    for (i <- array.indices) {
      array(i) = BigInt(data(i))
    }
    array
  }
}
