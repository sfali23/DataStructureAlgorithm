import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait LargestNumberImpl {

  private def max(n1: Int, n2: Int): Int = {
    if (n2 == Int.MinValue) n1
    else {
      val s1 = n1.toString
      val s2 = n2.toString

      val n3 = (s1 + s2).toInt
      val n4 = (s2 + s1).toInt

      if (n3 >= n4) n1 else n2
    }
  }

  def largestNumber(inputs: List[Int]): String = {
    val digits = inputs.to[ListBuffer]
    val answer = ListBuffer[Int]()
    while (digits.nonEmpty) {
      var maxDigit = Int.MinValue
      for (i <- digits.indices) {
        val current = digits(i)
        maxDigit = max(current, maxDigit)
      }
      answer += maxDigit
      digits -= maxDigit
    }
    answer.mkString("")
  }

  def readData: List[Int] = {
    val n = StdIn.readInt()
    readArray(n)
  }

  private def readArray(n: Int): List[Int] = {
    val result = Array.ofDim[Int](n)
    val line = StdIn.readLine()
    val splits = line.split(" ")
    for (i <- splits.indices) {
      result(i) = splits(i).toInt
    }
    result.toList
  }
}

object LargestNumber extends App with LargestNumberImpl {
  println(largestNumber(readData))
}
