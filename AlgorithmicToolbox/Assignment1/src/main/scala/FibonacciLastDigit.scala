import scala.io.StdIn

/**
  * @author sali
  */
object FibonacciLastDigit extends App {

  def getFibonacciLastDigit(n: Int): Int =
    if (n <= 1) n
    else {
      val array = Array.ofDim[Int](n + 1)
      array(0) = 0
      array(1) = 1
      for (i <- 2 to n)
        array(i) = (array(i - 1) + array(i - 2)) % 10
      array(n)
    }

  def getFibonacciSumLastDigit(n: Int): Int =
    if (n <= 1) n
    else {
      var sum = 1
      val array = Array.ofDim[Int](n + 1)
      array(0) = 0
      array(1) = 1
      for (i <- 2 to n) {
        val x = (array(i - 1) + array(i - 2)) % 10
        sum = (sum                            % 10) + (x % 10)
        array(i) = x
      }

      sum
    }

  val n = StdIn.readInt
  println(getFibonacciLastDigit(n))
}
