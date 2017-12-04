import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{ BeforeAndAfterAll, FunSuite }

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class BuildHeapTest extends FunSuite with BeforeAndAfterAll with BuildHeapWork with TestHelper {

  test("BuildHeap: Test case #1") {
    val swaps = generateSwaps(Array(5, 4, 3, 2, 1))
    assert(swaps.length === 3)
    assert(swaps.toArray.sorted === Array((1, 4), (0, 1), (1, 3)).sorted)
  }

  test("BuildHeap: Test case #2") {
    val swaps = generateSwaps(Array(1, 2, 3, 4, 5))
    assert(swaps.length === 0)
  }

  test("BuildHeap: pre-defined test") {
    for (i <- 1 to 6) {
      val fileNamePrefix = f"$i%02d"
      println(s"Running test: $fileNamePrefix")
      val lines = readLines(fileNamePrefix, None)
      val n = lines.head.toInt
      val input = lines.last
      val output = readLines(fileNamePrefix, Some("a"))
      val swaps = generateSwaps(readArray(n, input))
      assert(swaps.length === output.head.toInt)
      assert(swaps.toArray.sorted === toArray(output.tail))
    }
  }

  private def toArray(expected: List[String]): Array[(Int, Int)] =
    (for {
      s <- expected
      splits = s.split(" ")
      i = splits.head.toInt
      j = splits.last.toInt
    } yield (i, j)).toArray.sorted

  override val testDataFolderPath: String = "make-heap"
}
