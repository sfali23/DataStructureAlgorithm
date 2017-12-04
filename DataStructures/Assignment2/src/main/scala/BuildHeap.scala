import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

class Heap(array: Array[Int]) {

  private val maxSize = array.length
  private val _heap = array
  private var size: Int = maxSize - 1
  val swaps: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()

  def moveUp(i: Int): Unit = {
    var index = i
    while (index >= 0 && _heap(index) < _heap(parent(index))) {
      val tmp = _heap(parent(index))
      _heap(parent(index)) = _heap(index)
      _heap(index) = tmp
      index = parent(index)
    }
  }

  def moveDown(i: Int): Unit = {
    var minIndex = i
    val l = leftChild(i)
    minIndex = if (l <= size && _heap(l) < _heap(minIndex)) l else minIndex

    val r = rightChild(i)
    minIndex = if (r <= size && _heap(r) < _heap(minIndex)) r else minIndex

    if (i != minIndex) {
      swaps += ((i, minIndex))
      val tmp = _heap(i)
      _heap(i) = _heap(minIndex)
      _heap(minIndex) = tmp
      moveDown(minIndex)
    }
  }

  def insert(value: Int): Unit =
    if (size <= maxSize) {
      size += 1
      _heap(size) = value
      moveUp(size)
    }

  def extractMin: Int = {
    val result = _heap(0)
    _heap(0) = _heap(size)
    size -= 1
    moveDown(0)
    result
  }

  private def parent(i: Int): Int = (i - 1) / 2

  private def leftChild(i: Int) = 2 * i + 1

  private def rightChild(i: Int) = 2 * i + 2

}

object Heap {
  def apply(array: Array[Int]): Heap = new Heap(array)
}

trait BuildHeapWork {

  def readArray(n: Int, input: String): Array[Int] = {
    val result = Array.ofDim[Int](n)
    val splits = input.split(" ")
    for (i <- splits.indices) result(i) = splits(i).toInt
    result
  }

  def generateSwaps(array: Array[Int]): ArrayBuffer[(Int, Int)] = {
    val heap = Heap(array)
    for (i <- array.length / 2 to 0 by -1) heap.moveDown(i)
    heap.swaps
  }

}

object BuildHeap extends App with BuildHeapWork {
  private val n = StdIn.readInt()
  private val line = StdIn.readLine()
  val swaps = generateSwaps(readArray(n, line))
  println(swaps.length)
  if (swaps.nonEmpty) {
    swaps.foreach {
      case (i, j) => println(s"$i $j")
    }
  }
}
