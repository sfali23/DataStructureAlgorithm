import scala.io.StdIn

trait ClosestWork {

  implicit class SortByX(array: Array[(Long, Long)]) {

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

  private def closestPairs(points: Array[(Long, Long)]): Double =
    if (points.length <= 3) bruteForce(points)
    else {
      val mid = points.length / 2
      val leftMin = closestPairs(points.slice(0, mid))
      val rightMin = closestPairs(points.slice(mid, points.length))
      if (leftMin == 0.0) leftMin
      else if (rightMin == 0.0) rightMin
      else {
        val min =
          if (leftMin <= rightMin) leftMin else rightMin
        val splitMin = closestPairsSplit(points, min)
        if (splitMin <= leftMin && splitMin <= rightMin) splitMin
        else if (leftMin <= rightMin && leftMin < splitMin) leftMin
        else rightMin
      }
    }

  private def closestPairsSplit(points: Array[(Long, Long)], min: Double): Double = {
    val mid = points.length / 2
    val (midX, _) = points(mid)
    val strip = (for {
      i <- points.indices
      p = points(i)
      if math.abs(p._1 - midX) < min
    } yield p).toArray.sortByY()

    var currentMin = min

    for (i <- strip.indices) {
      var j = i + 1
      while (j < strip.length && math.abs(strip(j)._2 - strip(i)._2) < currentMin) {
        val dist = euclideanDistance(strip(i), strip(j))
        if (dist < currentMin) currentMin = dist
        j += 1
      }
    }
    currentMin
  }

  def minimumDistance(points: Array[(Long, Long)]): Double =
    BigDecimal(closestPairs(points.sortByX()))
      .setScale(4, BigDecimal.RoundingMode.HALF_UP)
      .toDouble

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
