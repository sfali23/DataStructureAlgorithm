import org.scalatest.FunSuite

/**
  * @author sali
  */
trait SortingTest extends FunSuite with SortingWork {

  test("Partition test case 1") {
    val array = Array[Int](1, 2, 3, 1, 2, 3, 1, 2, 3)
    val result = partition(array, 0, array.length - 1)
    assert((0, 2) === result)
  }

  test("Partition test case 2") {
    val array = Array[Int](3, 2, 1, 3, 2, 1, 3, 2, 1)
    val result = partition(array, 0, array.length - 1)
    assert((6, 8) === result)
  }

  test("Partition test case 3") {
    val array = Array[Int](2, 3, 1, 2, 1, 3, 2, 3, 1)
    val result = partition(array, 0, array.length - 1)
    assert((3, 5) === result)
  }

  test("Partition test case 4") {
    val array = Array[Int](2, 2, 2, 2, 2, 2, 2, 2, 2)
    val result = partition(array, 0, array.length - 1)
    assert((0, array.length - 1) === result)
  }

  test("randomizedQuickSort test case 1") {
    val array = Array[Int](1, 2, 3, 1, 2, 3, 1, 2, 3)
    randomizedQuickSort(array)
    assert(Array[Int](1, 1, 1, 2, 2, 2, 3, 3, 3) === array)
  }

  test("randomizedQuickSort test case 2") {
    val array = Array[Int](3, 2, 1, 3, 2, 1, 3, 2, 1)
    randomizedQuickSort(array)
    assert(Array[Int](1, 1, 1, 2, 2, 2, 3, 3, 3) === array)
  }

  test("randomizedQuickSort test case 3") {
    val array = Array[Int](2, 3, 1, 2, 1, 3, 2, 3, 1)
    randomizedQuickSort(array)
    assert(Array[Int](1, 1, 1, 2, 2, 2, 3, 3, 3) === array)
  }

  test("randomizedQuickSort test case 4") {
    val array = Array[Int](2, 3, 9, 2, 2)
    randomizedQuickSort(array)
    assert(Array[Int](2, 2, 2, 3, 9) === array)
  }
}
