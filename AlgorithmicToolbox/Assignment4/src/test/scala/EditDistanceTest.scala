import org.scalatest.FunSuite

/**
  * @author sali
  */
trait EditDistanceTest extends FunSuite with EditDistanceWork {

  test("EditDistance: Test case #1") {
    assert(editDistance("ab", "ab") === 0)
  }

  test("EditDistance: Test case #2") {
    assert(editDistance("short", "ports") === 3)
  }

  test("EditDistance: Test case #3") {
    assert(editDistance("editing", "distance") === 5)
  }
}
