import scala.io.StdIn

trait ClosestPairsWork {

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

  def bruteForce(points: Array[(Long, Long)]): (Double, (Long, Long), (Long, Long)) = {
    var minimum = Double.MaxValue
    var pair1 = (Long.MaxValue, Long.MaxValue)
    var pair2 = (Long.MaxValue, Long.MaxValue)
    for (i <- points.indices) {
      for (j <- i + 1 until points.length) {
        val distance = euclideanDistance(points(i), points(j))
        if (distance < minimum) {
          minimum = distance
          pair1 = points(i)
          pair2 = points(j)
        }
      }
    }
    (minimum, pair1, pair2)
  }

  private def closestPairs(points: Array[(Long, Long)]): (Double, (Long, Long), (Long, Long)) =
    if (points.length <= 3) bruteForce(points)
    else {
      val mid = points.length / 2
      val (leftMin, leftPair1, leftPair2) = closestPairs(points.slice(0, mid))
      val (rightMin, rightPair1, rightPair2) = closestPairs(points.slice(mid, points.length))
      if (leftMin == 0.0) (leftMin, leftPair1, leftPair2)
      else if (rightMin == 0.0) (rightMin, rightPair1, rightPair2)
      else {
        val (min, pair1, pair2) =
          if (leftMin <= rightMin) (leftMin, leftPair1, leftPair2) else (rightMin, rightPair1, rightPair2)
        val (splitMin, splitPair1, splitPair2) = closestPairsSplit(points, min, pair1, pair2)
        if (splitMin <= leftMin && splitMin <= rightMin) (splitMin, splitPair1, splitPair2)
        else if (leftMin <= rightMin && leftMin < splitMin) (leftMin, leftPair1, leftPair2)
        else (rightMin, rightPair1, rightPair2)
      }
    }

  private def closestPairsSplit(points: Array[(Long, Long)],
                                min: Double,
                                closestPair1: (Long, Long),
                                closestPair2: (Long, Long)): (Double, (Long, Long), (Long, Long)) = {
    val mid = points.length / 2
    val (midX, _) = points(mid)
    val strip = (for {
      i <- points.indices
      p = points(i)
      if math.abs(p._1 - midX) < min
    } yield p).toArray.sortByY()

    var currentMin = min
    var pair1 = closestPair1
    var pair2 = closestPair2

    for (i <- strip.indices) {
      var j = i + 1
      while (j < strip.length && math.abs(strip(j)._2 - strip(i)._2) < currentMin) {
        val dist = euclideanDistance(strip(i), strip(j))
        if (dist < currentMin) {
          currentMin = dist
          pair1 = strip(i)
          pair2 = strip(j)
        }
        j += 1
      }
    }
    (currentMin, pair1, pair2)
  }

  def minimumDistance(points: Array[(Long, Long)]): (Double, (Long, Long), (Long, Long)) =
    closestPairs(points.sortByX())

}

object ClosestPairs extends App with ClosestPairsWork {

  private val minDistance =
    BigDecimal(minimumDistance(readData)._1).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
  println(minDistance)

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
