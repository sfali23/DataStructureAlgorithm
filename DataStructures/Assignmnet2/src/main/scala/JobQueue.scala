class JobQueueHeap(maxSize: Int) {

  private var size = -1
  private val storage = Array.ofDim[Int](maxSize)

  def moveUp(i: Int): Unit = {
    var index = i
    while (index >= 0 && storage(index) < storage(parent(index))) {
      val parentIndex = parent(index)
      val tmp = storage(parentIndex)
      storage(parentIndex) = storage(index)
      storage(index) = tmp
      index = parentIndex
    }
  }

  def moveDown(i: Int): Unit = {
    var minIndex = i
    val l = leftChild(i)
    minIndex = if (l <= size && storage(l) < storage(minIndex)) l else minIndex

    val r = rightChild(i)
    minIndex = if (r <= size && storage(r) < storage(minIndex)) r else minIndex

    if (i != minIndex) {
      val tmp = storage(i)
      storage(i) = storage(minIndex)
      storage(minIndex) = tmp
      moveDown(minIndex)
    }
  }

  def insert(value: Int): Unit =
    if (size < maxSize) {
      size += 1
      storage(size) = value
      moveUp(size)
    }

  def extractMin: Int = {
    val result = storage(0)
    storage(0) = storage(size)
    size -= 1
    moveDown(0)
    result
  }

  def heap: Array[Int] = storage.take(size + 1)

  def print(): Unit = println(heap.mkString("[", ", ", "]"))

  private def parent(i: Int): Int = (i - 1) / 2
  private def leftChild(i: Int) = 2 * i + 1
  private def rightChild(i: Int) = 2 * i + 2

}

object JobQueueHeap {
  def apply(maxSize: Int): JobQueueHeap = new JobQueueHeap(maxSize)
}

trait JobQueueWork {}

/**
  * @author sali
  */
object JobQueue extends App with JobQueueWork {}
