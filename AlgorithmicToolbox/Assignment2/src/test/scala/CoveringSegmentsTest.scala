import org.scalatest.FunSuite

/**
  * @author sali
  */
trait CoveringSegmentsTest extends FunSuite with CoveringSegmentsImpl {

  test("CoveringSegments: Test case #1") {
    val result = optimalPoints(Array[(BigInt, BigInt)]((1, 3), (3, 6), (2, 5)))
  }
}
