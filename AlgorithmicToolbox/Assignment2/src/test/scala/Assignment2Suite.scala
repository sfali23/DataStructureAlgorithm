import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class Assignment2Suite extends ChangeTest
  with FractionalKnapsackTest
  with DotProductTest
  with CoveringSegmentsTest
  with DifferentSummandsTest
  with LargestNumberTest
