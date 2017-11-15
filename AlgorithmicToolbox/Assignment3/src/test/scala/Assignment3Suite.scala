import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class Assignment3Suite
    extends BinarySearchTest
    with SortingTest
    with InversionsTest
    with ClosestTest
    with MajorityElementTest
    with PointsAndSegmentsTest
