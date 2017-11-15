import scala.io.StdIn

trait MajorityElementWork {

  private def findCandidate(input: Array[Int]): (Int, Int) = {
    var count = 0
    var candidate = 0
    for (i <- input.indices) {
      if (count == 0) {
        candidate = input(i)
        count = 1
      } else if (count > 0) {
        if (input(i) == candidate) count += 1 else count -= 1
      }
    }
    (count, candidate)
  }

  private def checkCandidate(input: Array[Int], count: Int, candidate: Int): Int =
    if (count > 0) {
      val count = input.count(_ == candidate)
      val majority = count > input.length / 2
      if (majority) 1 else 0
    } else 0

  def getMajorityElement(input: Array[Int]): Int = {
    val (count, candidate) = findCandidate(input)
    checkCandidate(input, count, candidate)
  }
}

object MajorityElement extends App with MajorityElementWork {

  println(getMajorityElement(readData))

  private def readData: Array[Int] = {
    val n = StdIn.readInt()
    val array = Array.ofDim[Int](n)
    val line = StdIn.readLine()
    val data = line.split(" ")
    for (i <- array.indices) {
      array(i) = data(i).toInt
    }
    array
  }
}
