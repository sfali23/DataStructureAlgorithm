import org.junit.runner.RunWith
import org.scalatest.{ BeforeAndAfterAll, FunSuite }
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class JobQueueTest extends FunSuite with BeforeAndAfterAll with JobQueueWork with TestHelper {

  test("build heap") {
    val heap = JobQueueHeap(13)

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
  }

  test("extract min") {
    val heap = JobQueueHeap(13)

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
  }
}
