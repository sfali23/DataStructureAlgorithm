import org.scalatest.FunSuite

/**
  * @author sali
  */
trait FractionalKnapsackTest extends FunSuite {
  private val delta = 1.0E-3

  test("Knapscak with 1 item to be fully filled") {
    val valuesAndWeights = Array[(Double, Double)]((200, 10))
    val result = TestFractionalKnapsack.getOptimalValue(10, valuesAndWeights)
    assert((result - 200.0000) <= delta)
  }

  test("Knapscak with 1 item to be partially filled") {
    val valuesAndWeights = Array[(Double, Double)]((500, 30))
    val result = TestFractionalKnapsack.getOptimalValue(10, valuesAndWeights)
    assert((result - 166.6667) <= delta)
  }

  test("Knapscak process multiple items") {
    val valuesAndWeights = Array[(Double, Double)]((120, 30), (60, 20), (100, 50))
    val result = TestFractionalKnapsack.getOptimalValue(50, valuesAndWeights)
    assert((result - 180.0000) <= delta)
  }
}

private object TestFractionalKnapsack extends FractionalKnapsackImpl
