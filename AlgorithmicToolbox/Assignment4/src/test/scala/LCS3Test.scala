import org.scalatest.FunSuite

/**
  * @author sali
  */
trait LCS3Test extends FunSuite with LCS3Work {

  test("LCS2: Test case #1") {
    val result = lcs2(Array(1, 2, 3), Array(2, 1, 3))
    assert(result === Array[Long](2, 3))
  }

  test("LCS2: Test case #1.1") {
    val result = lcs2(Array(2, 1, 3), Array(1, 2, 3))
    assert(result === Array[Long](1, 3))
  }

  test("LCS2: Test case #2") {
    val result = lcs2(Array(1, 2, 3), Array(1, 3, 5))
    assert(result === Array[Long](1, 3))
  }

  test("LCS2: Test case #3") {
    val result = lcs2(Array(2, 1, 3), Array(1, 3, 5))
    assert(result === Array[Long](1, 3))
  }

  test("LCS2: Test case #4") {
    val result = lcs2(Array(8, 3, 2, 1, 7), Array(8, 2, 1, 3, 8, 10, 7))
    assert(result === Array[Long](8, 2, 1, 7))
  }

  test("LCS3: Test case #1") {
    val result = lcs3(Array(1, 2, 3), Array(2, 1, 3), Array(1, 3, 5))
    assert(result === 2)
  }

  test("LCS3: Test case #2") {
    val result = lcs3(Array(8, 3, 2, 1, 7),
                      Array(8, 2, 1, 3, 8, 10, 7),
                      Array(6, 8, 3, 1, 4, 7))
    assert(result === 3)
  }

}
