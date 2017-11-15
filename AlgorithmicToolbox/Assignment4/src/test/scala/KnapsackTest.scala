import org.scalatest.FunSuite

/**
  * @author sali
  */
trait KnapsackTest extends FunSuite with KnapsackWork {

  test("Knapsack: Test case #1") {
    assert(optimalWeight(10, Array((1, 1), (4, 4), (8, 8))) === 9)
  }

  test("Knapsack: Test case #2") {
    assert(optimalWeight(9, Array((1, 1), (2, 4), (3, 4), (4, 5), (5, 7))) === 13)
  }
}
