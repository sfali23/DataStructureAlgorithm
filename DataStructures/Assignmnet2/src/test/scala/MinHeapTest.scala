import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class MinHeapTest extends FunSuite {

  test("build heap") {
    val heap = MinHeap(13)
    heap.insertAll(42, 29, 18, 14, 7, 18, 12, 11, 13)
    assert(heap.heap === Array(7, 11, 12, 13, 18, 29, 18, 42, 14))
  }

  test("extract min") {
    val heap = MinHeap(13)
    heap.insertAll(42, 29, 18, 14, 7, 18, 12, 11, 13)

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

  test("extract and insert min") {
    val heap = MinHeap(13)
    heap.insertAll(0, 1, 2, 3, 4, 5)
    assert(heap.heap === Array(0, 1, 2, 3, 4, 5))

    var min = heap.extractMin
    assert(min === 0)
    assert(heap.heap === Array(1, 3, 2, 5, 4))

    min = heap.extractMin
    assert(min === 1)
    assert(heap.heap === Array(2, 3, 4, 5))

    heap.insertAll(0, 1)
    assert(heap.heap === Array(0, 2, 1, 5, 3, 4))
  }

}
