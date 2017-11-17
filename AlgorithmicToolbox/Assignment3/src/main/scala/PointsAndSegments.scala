import scala.Ordering.Implicits._
import scala.io.StdIn

trait PointsAndSegmentsWork[T] {

  private def searchAndCount(segments: Array[(T, T)], low: Int, high: Int, point: T)(
    implicit numeric: Numeric[T]
  ): Long =
    // if this point is less than the lower bound value of first segment then
    // this point is not it the any of segments
    if (point < segments(low)._1) 0
    else {
      val mid = low + ((high - low) / 2)
      val (lowerBound, upperBound) = segments(mid)
      val newCount = if (lowerBound <= point && point <= upperBound) 1 else 0
      if (low == high) newCount
      else {
        val leftHigh = mid - 1
        val leftCount =
          if (low > leftHigh || leftHigh < 0 || point < segments(low)._1) 0
          else searchAndCount(segments, low, leftHigh, point)

        val rightLow = mid + 1
        val rightCount =
          if (rightLow > high || rightLow >= segments.length || point < segments(rightLow)._1) 0
          else searchAndCount(segments, rightLow, high, point)

        newCount + leftCount + rightCount
      }
    }

  private def searchAndCount(segments: Array[(T, T)], point: T)(implicit numeric: Numeric[T]): Long =
    searchAndCount(segments, 0, segments.length - 1, point)

  def countSegments(segments: Array[(T, T)], points: Array[T])(implicit numeric: Numeric[T]): Array[Long] = {
    val sortedSegments = segments.sorted
    for {
      point <- points
      count = searchAndCount(sortedSegments, point)
    } yield count
  }

  def naiveCountSegments(segments: Array[(T, T)], points: Array[T])(implicit numeric: Numeric[T]): Array[Long] = {
    val sortedSegments = segments.sorted
    val counts = Array.ofDim[Long](points.length)
    for (i <- points.indices) {
      for (j <- sortedSegments.indices) {
        val segment = sortedSegments(j)
        if (segment._1 <= points(i) && points(i) <= segment._2) {
          counts(i) = counts(i) + 1
        }
      }
    }

    counts
  }
}

object PointsAndSegments extends App with PointsAndSegmentsWork[BigInt] {

  val (segments, points) = readData
  val result = countSegments(segments, points)
  println(result.mkString(" "))

  private def readData = {
    var line = StdIn.readLine
    var values = line.split(" ")
    val s = values.head.toInt
    val p = values.last.toInt

    val segments = Array.ofDim[(BigInt, BigInt)](s)
    for (i <- segments.indices) {
      line = StdIn.readLine
      values = line.split(" ")
      segments(i) = (BigInt(values.head), BigInt(values.last))
    }

    line = StdIn.readLine
    values = line.split(" ")
    val points = Array.ofDim[BigInt](p)
    for (i <- points.indices) {
      points(i) = BigInt(values(i))
    }

    (segments, points)
  }
}
