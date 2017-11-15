import scala.Ordering.Implicits._
import scala.io.StdIn

trait BinarySearchWork[T] {

  private def binarySearch(source: Array[T], low: Int, high: Int, x: T)(implicit numeric: Numeric[T]): Int =
    if (x < source(low) || x > source(high)) -1
    else {
      val mid = low + ((high - low) / 2)
      val current = source(mid)
      if (current == x) mid
      else if (x < current) binarySearch(source, low, mid - 1, x)
      else binarySearch(source, mid + 1, high, x)
    }

  def binarySearch(source: Array[T], x: T)(implicit numeric: Numeric[T]): Int =
    binarySearch(source, 0, source.length - 1, x)

  def binarySearch(source: Array[T], search: Array[T])(implicit numeric: Numeric[T]): Seq[Int] =
    for {
      x <- search.indices
      index = binarySearch(source, search(x))
    } yield index

}

object BinarySearch extends App with BinarySearchWork[BigInt] {
  val (source, search) = readInput
  println(binarySearch(source, search).mkString(" "))

  private def parseLine(line: String): Array[BigInt] = {
    val data = line.split(" ")
    val n = data(0).toInt
    val result = Array.ofDim[BigInt](n)
    for (i <- result.indices) {
      result(i) = BigInt(data(i + 1))
    }
    result
  }

  private def readInput: (Array[BigInt], Array[BigInt]) = (parseLine(StdIn.readLine()), parseLine(StdIn.readLine()))

}
