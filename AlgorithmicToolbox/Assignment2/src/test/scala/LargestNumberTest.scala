import org.scalatest.FunSuite

/**
  * @author sali
  */
trait LargestNumberTest extends FunSuite with LargestNumberImpl {

  test("LargestNumberTest: Test case #1") {
    val result = largestNumber(21 :: 2 :: Nil)
    assert(result === "221")
  }

  test("LargestNumberTest: Test case #2") {
    val result = largestNumber(9 :: 4 :: 6 :: 1 :: 9 :: Nil)
    assert(result === "99641")
  }

  test("LargestNumberTest: Test case #3") {
    val result = largestNumber(23 :: 39 :: 92 :: Nil)
    assert(result === "923923")
  }
}
