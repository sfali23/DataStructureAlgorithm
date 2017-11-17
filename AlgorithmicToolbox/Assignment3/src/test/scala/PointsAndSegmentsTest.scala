import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
trait PointsAndSegmentsTest extends FunSuite with PointsAndSegmentsWork[Int] with DataGenerator {

  test("countSegments: Test case 1") {
    val segments = Array((0, 5), (-3, 2), (7, 10))
    val points = Array(1, 6)
    assert(countSegments(segments, points) === Array(2, 0))
  }

  test("countSegments: Test case 2") {
    val segments = Array((2, 3), (0, 5), (7, 10))
    val points = Array(1, 6, 11)
    assert(countSegments(segments, points) === Array(1, 0, 0))
  }

  test("countSegments: Test case 3") {
    val segments = Array((-10, 10))
    val points = Array(-100, 100, 0)
    assert(countSegments(segments, points) === Array(0, 0, 1))
  }
}

@RunWith(classOf[JUnitRunner])
class DefaultPointsAndSegmentsTest extends PointsAndSegmentsTest {

  private val start = -100000000
  private val end = 100000000
  private val totalNumOfSegments = 50000

  test("countSegments: Max") {
    val (segments, points) = generateData(totalNumOfSegments, totalNumOfSegments, start, end)

    println(
      s"Running countSegments with number of segments = $totalNumOfSegments and number of points = $totalNumOfSegments"
    )

    val countsByBinarySearch = countSegmentsByBinarySearch(segments, points)
    val countsByNaiveImplementation = countSegmentsByNaiveImplementation(segments, points)
    assert(countsByBinarySearch === countsByNaiveImplementation)
  }

  test("countSegments: random tests") {
    for (n <- 1 to 100) {
      val numOfSegments = generateInt(totalNumOfSegments)
      val numOfPoints = generateInt(totalNumOfSegments)

      val (segments, points) = generateData(numOfSegments, numOfPoints, start, end)

      println(s"Running test case #: $n")
      println(s"Running countSegments with number of segments = $numOfSegments and number of points = $numOfPoints")

      val countsByBinarySearch = countSegmentsByBinarySearch(segments, points)
      val countsByNaiveImplementation = countSegmentsByNaiveImplementation(segments, points)
      assert(countsByBinarySearch === countsByNaiveImplementation)
    }
  }

  private def countSegmentsByBinarySearch(segments: Array[(Int, Int)], points: Array[Int]) = {
    val startTime = System.nanoTime()
    val counts = countSegments(segments, points)
    printTimeElapsed(startTime, "Time taken to run count segments by binary search")
    counts
  }

  private def countSegmentsByNaiveImplementation(segments: Array[(Int, Int)], points: Array[Int]) = {
    val startTime = System.nanoTime()
    val counts = naiveCountSegments(segments, points)
    printTimeElapsed(startTime, "Time taken to run count segments by naive algorithm")
    counts
  }

  private def generateData(numOfSegments: Int,
                           numOfPoints: Int,
                           start: Int,
                           end: Int): (Array[(Int, Int)], Array[Int]) = {
    val segments = generateSegments(numOfSegments, start, end)
    val points = generatePoints(numOfPoints, start, end).sorted
    (segments, points)
  }

  private def generateSegments(numOfSegments: Int, start: Int, end: Int): Array[(Int, Int)] =
    (for {
      _ <- 1 to numOfSegments
      segment = generateSegment(start, end)
    } yield segment).toArray

  private def generateSegment(start: Int, end: Int): (Int, Int) = {
    val lowerBound = generateInt(start, end)
    var upperBound = generateInt(start, end)
    while (upperBound < lowerBound) upperBound = generateInt(start, end)
    (lowerBound, upperBound)
  }

}
