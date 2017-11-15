import org.scalatest.FunSuite

/**
  * @author sali
  */
trait InversionsTest extends FunSuite with InversionsWork[Int] {

  override val debug: Boolean = false

  test("countSplitInversions: Test case 1") {
    val result = countSplitInversions(Array[Int](1, 3, 5), Array[Int](2, 4, 6))
    assert(result._2 === 3L)
  }

  test("countSplitInversions: Test case 2") {
    val result = countSplitInversions(Array[Int](2, 3, 9), Array[Int](2, 9))
    assert(result._2 === 2L)
  }

  test("sortAndCountInversions: Test case 1") {
    val result = sortAndCountInversions(Array[Int](1, 3, 5, 2, 4, 6))
    assert(result._2 === 3L)
  }

  test("sortAndCountInversions: Test case 2") {
    val result = sortAndCountInversions(Array[Int](2, 3, 9, 2, 9))
    assert(result._2 === 2L)
  }
}
