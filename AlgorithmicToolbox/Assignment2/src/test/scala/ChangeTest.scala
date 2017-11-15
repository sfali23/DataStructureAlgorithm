import org.scalatest.FunSuite

/**
  * @author sali
  */
trait ChangeTest extends FunSuite {

  test("Given input value of 0 will return empty list of denomination") {
    val denominations = TestChange.getDenominations(0)
    assert(denominations === Nil)
  }

  test("Given input value of 10 will return List(10)") {
    val denominations = TestChange.getDenominations(10)
    assert(denominations === List(10))
  }

  test("Given input value of 5 will return List(5)") {
    val denominations = TestChange.getDenominations(5)
    assert(denominations === List(5))
  }

  test("Given input value of 1 will return List(1)") {
    val denominations = TestChange.getDenominations(1)
    assert(denominations === List(1))
  }

  test("Given input value of 2 will return List(1,1)") {
    val denominations = TestChange.getDenominations(2)
    assert(denominations === List(1, 1))
  }

  test("Given input value of 28 will return List(10,10,5,1,1,1)") {
    val denominations = TestChange.getDenominations(28)
    assert(denominations === List(10, 10, 5, 1, 1, 1))
  }

  test("Given input value of 1000 will return List with only 10s") {
    val denominations = TestChange.getDenominations(1000)
    assert(denominations === List.fill(100)(10))
  }

  test("Given input value of 7 will return List should not contain 10") {
    val denominations = TestChange.getDenominations(7)
    assert(denominations === List(5, 1, 1))
  }

  test("Given input value of 12 will return List should not contain 5") {
    val denominations = TestChange.getDenominations(12)
    assert(denominations === List(10, 1, 1))
  }

  test("Given input value of 0 will return 0 minimum number of coins") {
    val change = TestChange.getChange(0)
    assert(change === 0)
  }
}

private object TestChange extends ChangeImpl
