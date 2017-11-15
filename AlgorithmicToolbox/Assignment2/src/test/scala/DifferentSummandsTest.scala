import org.scalatest.FunSuite

/**
  * @author sali
  */
trait DifferentSummandsTest extends FunSuite with DifferentSummandsImpl {

  test("DifferentSummands: Test case #1") {
    val result = differentSummands(2)
    assert(result === List(2))
  }

  test("DifferentSummands: Test case #2") {
    val result = differentSummands(6)
    assert(result === List(1, 2, 3))
  }

  test("DifferentSummands: Test case #3") {
    val result = differentSummands(8)
    assert(result === List(1, 2, 5))
  }

  test("DifferentSummands: Test case #4") {
    val result = differentSummands(10)
    assert(result === List(1, 2, 3, 4))
  }

  test("DifferentSummands: Test case #5") {
    val result = differentSummands(BigInt(1.0E9.toLong))
    assert(result.size === 44720)
  }

}
