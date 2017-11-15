import org.scalatest.FunSuite

/**
  * @author sali
  */
trait PlacingParenthesesTest extends FunSuite with PlacingParenthesesWork {

  test("PlacingParentheses: Test case #1") {
    assert(getMaximumValue("1+5") === 6)
  }

  test("PlacingParentheses: Test case #2") {
    assert(getMaximumValue("5-8+7*4-8+9") === 200)
  }

}
