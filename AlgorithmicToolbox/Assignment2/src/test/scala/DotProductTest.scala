import org.scalatest.FunSuite

import scala.util.Random

/**
  * @author sali
  */
trait DotProductTest extends FunSuite {

  private val start = -100000
  private val end = 100000
  private val rand = new Random()

  test("DotProduct: Test case #1") {
    val result = TestDotProduct.maxDotProduct(Array(BigInt(23)), Array(BigInt(39)))
    assert(result === 897)
  }

  test("DotProduct: Test case #2") {
    val a = Array(BigInt(1), BigInt(3), BigInt(-5))
    val b = Array(BigInt(-2), BigInt(4), BigInt(1))
    val result = TestDotProduct.maxDotProduct(a, b)
    assert(result === 23)
  }

  test("DotProduct: Test case #3, generated data with small n = 5") {
    val (a, b) = generateData(5)
    val result = TestDotProduct.maxDotProduct(a, b)
    println(s"With n = 5, result is: $result")
  }

  test("DotProduct: Test case #4, generated data with small n = 10") {
    val (a, b) = generateData(10)
    val result = TestDotProduct.maxDotProduct(a, b)
    println(s"With n = 10, result is: $result")
  }

  test("DotProduct: Test case #5, generated data random n") {
    val (a, b) = generateData
    val result = TestDotProduct.maxDotProduct(a, b)
    println(s"With n = 10, result is: $result")
  }

  test("DotProduct: Test case #6") {
    val a = Array(BigInt(1), BigInt(2), BigInt(3), BigInt(4), BigInt(5))
    val b = Array(BigInt(1), BigInt(0), BigInt(1), BigInt(0), BigInt(1))
    val result = TestDotProduct.maxDotProduct(a, b)
    assert(result === 12)
  }

  private def generateArray(n: Int): Array[BigInt] = {
    val result = Array.ofDim[BigInt](n)
    for (i <- 0 until n) {
      val value = start + rand.nextInt(end - start)
      result(i) = BigInt(value)
    }
    val str = result.mkString("[", ", ", "]")
    println(str)
    result
  }

  private def generateData(n: Int): (Array[BigInt], Array[BigInt]) = {
    println(s"Generating data for $n number of items")
    val a = generateArray(n)
    val b = generateArray(n)
    (a, b)
  }

  private def generateData: (Array[BigInt], Array[BigInt]) =
    generateData(rand.nextInt(1000) + 1)

}

private object TestDotProduct extends DotProductImpl
