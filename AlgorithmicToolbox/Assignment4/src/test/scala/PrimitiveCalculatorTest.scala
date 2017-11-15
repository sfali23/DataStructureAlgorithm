import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
trait PrimitiveCalculatorTest extends FunSuite with PrimitiveCalculatorWork {

  test("PrimitiveCalculator: Test case #1") {
    val list = optimalSequence(1)
    assert(list.length === 1)
    assert(list === Array(1))
  }

  test("PrimitiveCalculator: Test case #2") {
    val list = optimalSequence(2)
    assert(list.length === 2)
    assert(list === Array(1, 2))
  }

  test("PrimitiveCalculator: Test case #3") {
    val list = optimalSequence(3)
    assert(list.length === 2)
    assert(list === Array(1, 3))
  }

  test("PrimitiveCalculator: Test case #4") {
    val list = optimalSequence(5)
    assert(list.length === 4)
    assert(list === Array(1, 2, 4, 5))
  }

  test("PrimitiveCalculator: Test case #5") {
    val list = optimalSequence(96234)
    assert(list.length === 15)
    assert(
      list === Array(1, 3, 9, 10, 11, 22, 66, 198, 594, 1782, 5346, 16038,
        16039, 32078, 96234))
  }

  test("PrimitiveCalculator: Test case #6") {
    val list = optimalSequence(10)
    assert(list.length === 4)
    assert(list === Array(1, 3, 9, 10))
  }

  test("PrimitiveCalculator: Test case #7") {
    val list = optimalSequence(1000000)
    assert(list.length === 20)
    assert(
      list === Array(1, 2, 4, 8, 24, 72, 216, 217, 434, 868, 1736, 5208, 15624,
        15625, 31250, 62500, 125000, 250000, 500000, 1000000))
  }

  /*test("PrimitiveCalculator: Test case max") {
    val list = optimalSequence(1000000)
    println(list.length)
    /*assert(list.length === 4)
    assert(list === Array(1, 3, 9, 10))*/
  }*/
}
