import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class TreeHeightTest
    extends FunSuite
    with TreeHeightWork
    with BeforeAndAfterAll
    with TestHelper {

  private var startTime: Long = 0L

  override protected def beforeAll(): Unit = {
    startTime = System.nanoTime()
  }

  override protected def afterAll(): Unit = {
    printTimeElapsed(startTime, "time to run all tests")
  }

  test("TreeHeight: Test case #1") {
    val root = readTree(5, "4 -1 4 1 1")
    assert(computeHeight(root) === 3)
  }

  test("TreeHeight: Test case #2") {
    val root = readTree(5, "-1 0 4 0 3")
    assert(computeHeight(root) === 4)
  }

  test("TreeHeight: read pre-defined tests") {
    for (i <- 1 to 24) {
      val fileNamePrefix = f"$i%02d"
      println(s"Running test: $fileNamePrefix")
      val lines = readLines(s"/tree-height/$fileNamePrefix")
      val n = lines.head.toInt
      val input = lines.last
      val output = readLines(s"/tree-height/$fileNamePrefix.a").head.toInt
      assert(computeHeight(readTree(n, input)) === output)
    }
  }
}
