import scala.io.StdIn

trait ClosestWork {

  implicit class SortUtil(array: Array[(Long, Long)]) {

    def sortByX(): Array[(Long, Long)] = array.sortWith {
      case ((x1, _), (x2, _)) => x1 < x2
    }

    def sortByY(): Array[(Long, Long)] = array.sortWith {
      case ((_, y1), (_, y2)) => y1 < y2
    }

  } // end of implicit class

  private def euclideanDistance(p1: (Long, Long), p2: (Long, Long)): Double =
    math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))

  def bruteForce(points: Array[(Long, Long)]): Double = {
    var minimum = Double.MaxValue
    for (i <- points.indices) {
      for (j <- i + 1 until points.length) {
        val distance = euclideanDistance(points(i), points(j))
        if (distance < minimum) minimum = distance
      }
    }
    minimum
  }

  private def closestPairs(X: Array[(Long, Long)], Y: Array[(Long, Long)]): Double =
    if (X.length <= 3) bruteForce(X)
    else {
      val mid = X.length / 2
      val Xl = X.slice(0, mid)
      val Xr = X.slice(mid, X.length)
      val Yl = Y.intersect(Xl)
      val Yr = Y.intersect(Xr)

      val leftMin = closestPairs(Xl, Yl)
      val rightMin = closestPairs(Xr, Yr)

      val min = leftMin.min(rightMin)
      if (min == 0.0) min
      else {
        val splitMin = closestPairsSplit(Y, X(X.length / 2)._1, min)
        if (splitMin <= leftMin && splitMin <= rightMin) splitMin
        else if (leftMin <= rightMin && leftMin < splitMin) leftMin
        else rightMin
      }
    }

  private def closestPairsSplit(points: Array[(Long, Long)], midX: Long, min: Double): Double = {
    val strip = points.filter(p => math.abs(p._1 - midX) < min)

    var currentMin = min

    for (i <- strip.indices) {
      var j = i + 1
      while (j < strip.length && math.abs(strip(j)._2 - strip(i)._2) < currentMin) {
        val dist = euclideanDistance(strip(i), strip(j))
        currentMin = if (dist < currentMin) dist else currentMin
        j += 1
      }
    }
    currentMin
  }

  def minimumDistance(points: Array[(Long, Long)]): Double =
    BigDecimal(closestPairs(points.sortByX(), points.sortByY())).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble

}

object Closest extends App with ClosestWork {

  println(minimumDistance(readData))

  private def readData: Array[(Long, Long)] = {
    val n = StdIn.readInt()
    (for {
      _ <- 0 until n
      line = StdIn.readLine()
      split = line.split(" ")
      pair = (split.head.toLong, split.last.toLong)
    } yield pair).toArray
  }
}
