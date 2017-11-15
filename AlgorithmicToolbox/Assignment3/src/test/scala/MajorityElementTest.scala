import org.scalatest.FunSuite

/**
  * @author sali
  */
trait MajorityElementTest extends FunSuite with MajorityElementWork {

  test("MajorityElement: Test case 1") {
    val majorityElement = getMajorityElement(Array(2, 3, 9, 2, 2))
    assert(majorityElement === 1)
  }

  test("MajorityElement: Test case 2") {
    val majorityElement = getMajorityElement(Array(1, 2, 3, 4))
    assert(majorityElement === 0)
  }

  test("MajorityElement: Test case 3") {
    val majorityElement = getMajorityElement(Array(1, 2, 3, 1))
    assert(majorityElement === 0)
  }

  test("MajorityElement: Test case 4") {
    val majorityElement = getMajorityElement(Array(2, 124554847, 2, 941795895, 2, 2, 2, 2, 792755190, 756617003))
    assert(majorityElement === 1)
  }
}
