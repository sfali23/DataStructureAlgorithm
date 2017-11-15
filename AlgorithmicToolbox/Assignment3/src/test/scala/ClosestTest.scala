import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
trait ClosestTest extends FunSuite with ClosestWork with DataGenerator {

  test("Closest: Test case #1") {
    val points: Array[(Long, Long)] = Array((0, 0), (3, 4))

    val min = minimumDistance(points)
    assert(min === 5.0)
  }

  test("Closest: Test case #2") {
    val points: Array[(Long, Long)] = Array((7, 7), (1, 100), (4, 8), (7, 7))

    val min = minimumDistance(points)
    assert(min === 0.0)
  }

  test("Closest: Test case #3") {
    val points: Array[(Long, Long)] =
      Array((4, 4), (-2, -2), (-3, -4), (-1, 3), (2, 3), (-4, 0), (1, 1), (-1, -1), (3, -1), (-4, 2), (-2, 4))

    val min = minimumDistance(points)
    assert(min === 1.4142)
  }

}

@RunWith(classOf[JUnitRunner])
class DefaultClosestTest extends ClosestTest {

  test("Closest: random tests") {
    for (i <- 1 to 10000) {
      val n = generateInt(2, 1000)
      val points = generatePairs(n, -100000, 10000)
      println(s"Running test case#: $i")
      var startTime = System.nanoTime()
      val min = minimumDistance(points)
      printTimeElapsed(startTime, "Time elapsed with DNC")
      startTime = System.nanoTime()
      val min_bf = BigDecimal(bruteForce(points.sorted)).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
      printTimeElapsed(startTime, "Time elapsed with brute force")
      if (min != min_bf) {
        println(s"Failed test # $i:-")
        println(s"Result for $n points using divide and conquer, min = $min")
        println(s"Result for $n points using brute force, min = $min_bf")
        println()
        println(points.mkString(" "))
        println(points.sortByX().mkString(" "))
      }
      assert(min === min_bf)
    }
  }

  test("Closest: max n") {
    val n = 100000
    val points = generatePairs(n, -1000000000, 1000000000)
    var startTime = System.nanoTime()
    val min = minimumDistance(points)
    printTimeElapsed(startTime, "Time elapsed with DNC")
    startTime = System.nanoTime()
    val min_bf = BigDecimal(bruteForce(points.sorted)).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
    printTimeElapsed(startTime, "Time elapsed with brute force")
    println(s"Result for $n points, min = $min")
    println(s"Result for $n points, min = $min_bf")
    assert(min === min_bf)
  }
}
