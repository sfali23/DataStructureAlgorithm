import org.scalatest.FunSuite

import scala.util.Random

/**
  * @author sali
  */
trait BinarySearchTest extends FunSuite with BinarySearchWork[Int] {

  private val rand = new Random()
  private val source = Array(1, 5, 8, 12, 13)
  private val search = Array(8, 1, 23, 1, 11)

  test("BinarySearch: Test case #1: search value lower then first element") {
    assert(binarySearch(source, 0) === -1)
  }

  test("BinarySearch: Test case #2: search value greater then last element") {
    assert(binarySearch(source, 23) === -1)
  }

  test("BinarySearch: Test case #3: search non-existing element") {
    assert(binarySearch(source, 11) === -1)
  }

  test("BinarySearch: Test case #4: search first element") {
    assert(binarySearch(source, 1) === 0)
  }

  test("BinarySearch: Test case #5: search last element") {
    assert(binarySearch(source, 13) === 4)
  }

  test("BinarySearch: Test case #6: search mid element") {
    assert(binarySearch(source, 8) === 2)
  }

  test("BinarySearch: Test case #7: search multi elements") {
    assert(binarySearch(source, search) === Seq(2, 0, -1, 0, -1))
  }

  /*test("BinarySearch: Test case #8: search random") {
    val source = generateSourceArray(51, 15)
    val search = generateSearchArray(51, 15)
    printArray(source)
    printArray(search)
    val result = binarySearch(source, search)
    printArray(result.toArray)
  }
   */
  private def generateArray(start: Int, end: Int, n: Int, sort: Boolean): Array[BigInt] = {
    val result = Array.ofDim[BigInt](n)
    for (i <- 0 until n) {
      val value = getRandomNumberInRange(start, end)
      result(i) = BigInt(value)
    }
    if (sort) result.sorted else result
  }

  private def getRandomNumberInRange(start: Int, end: Int): Int = start + rand.nextInt(end - start)

  private def generateSourceArray(end: Int, n: Int): Array[BigInt] = generateArray(1, end, n, sort = true)

  private def generateSearchArray(end: Int, n: Int): Array[BigInt] = generateArray(0, end, n, sort = false)

  private def generateSearchArray(source: Array[BigInt]): Array[BigInt] = {
    val n = source.length
    val result = Array.ofDim[BigInt](n)

    result
  }

  private def printArray[T](array: Array[T]): Unit = println(array.mkString(" "))

}
