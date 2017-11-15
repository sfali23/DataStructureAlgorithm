import scala.Ordering.Implicits._
import scala.io.StdIn
import scala.reflect.ClassTag

trait InversionsWork[T] {

  val debug: Boolean = false

  def countSplitInversions(left: Array[T], right: Array[T])(implicit numeric: Numeric[T],
                                                            classTag: ClassTag[T]): (Array[T], Long) = {
    var i = 0
    var j = 0
    val result = new Array[T](left.length + right.length)
    var inversionCount = 0L
    // val inversions = ArrayBuffer[(T, T)]()
    for (k <- result.indices) {
      if (j >= right.length) {
        // we have finished scanning right array, just copy over elements from left array
        result(k) = left(i)
        i += 1
      } else if (i >= left.length) {
        // we have finished scanning left array, just copy over elements from right array
        result(k) = right(j)
        j += 1
      } else if (left(i) <= right(j)) {
        // no inversion
        result(k) = left(i)
        i += 1
      } else {
        // inversion
        val t = right(j)
        inversionCount += (left.length - i)

        /*if (debug) {
          inversions ++= left.slice(i, left.length).foldLeft(ArrayBuffer[(T, T)]()) {
            case (buffer, e) => buffer += ((e, t))
          }
        }*/

        result(k) = t
        j += 1
      }
    }
    /*if (debug) {
      println(inversions.mkString(","))
    }*/

    (result, inversionCount)
  }

  def sortAndCountInversions(a: Array[T])(implicit numeric: Numeric[T], classTag: ClassTag[T]): (Array[T], Long) =
    if (a.length == 1) (a, 0)
    else {
      val m = a.length / 2
      val (b, c1) = sortAndCountInversions(a.slice(0, m))
      val (c, c2) = sortAndCountInversions(a.slice(m, a.length))
      val (d, c3) = countSplitInversions(b, c)
      (d, c1 + c2 + c3)
    }

}

object Inversions extends App with InversionsWork[BigInt] {
  private val array = readData
  private val (_, count) = sortAndCountInversions(array)
  println(count)

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
