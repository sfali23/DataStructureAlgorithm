import org.junit.runner.RunWith
import org.scalatest.{ BeforeAndAfterAll, FunSuite }
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class BuildHeapTest extends FunSuite with BeforeAndAfterAll with BuildHeapWork with TestHelper {

  /*test("build heap") {
    val heap = Heap(13)

    heap.insert(42)
    heap.insert(29)
    heap.insert(18)
    heap.insert(14)
    heap.insert(7)
    heap.insert(18)
    heap.insert(12)
    heap.insert(11)
    heap.insert(13)
    assert(heap.heap === Array(7, 11, 12, 13, 18, 29, 18, 42, 14))
  }*/

  /*test("extract min"){
    val heap = Heap(13)

    heap.insert(42)
    heap.insert(29)
    heap.insert(18)
    heap.insert(14)
    heap.insert(7)
    heap.insert(18)
    heap.insert(12)
    heap.insert(11)
    heap.insert(13)

    var min = heap.extractMin
    assert(min === 7)
    assert(heap.heap === Array(11, 13, 12, 14, 18, 29, 18, 42))

    min = heap.extractMin
    assert(min === 11)
    assert(heap.heap === Array(12, 13, 18, 14, 18, 29, 42))

    min = heap.extractMin
    assert(min === 12)
    assert(heap.heap === Array(13, 14, 18, 42, 18, 29))

    min = heap.extractMin
    assert(min === 13)
    assert(heap.heap === Array(14, 18, 18, 42, 29))

    min = heap.extractMin
    assert(min === 14)
    assert(heap.heap === Array(18, 29, 18, 42))

    min = heap.extractMin
    assert(min === 18)
    assert(heap.heap === Array(18, 29, 42))

    min = heap.extractMin
    assert(min === 18)
    assert(heap.heap === Array(29, 42))

    min = heap.extractMin
    assert(min === 29)
    assert(heap.heap === Array(42))

    min = heap.extractMin
    assert(min === 42)
    assert(heap.heap === Array())
  }*/

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
      val lines = readLines(s"/make-heap/$fileNamePrefix")
      val n = lines.head.toInt
      val input = lines.last
      val output = readLines(s"/make-heap/$fileNamePrefix.a")
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

}
