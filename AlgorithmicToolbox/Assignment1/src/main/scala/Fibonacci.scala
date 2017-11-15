import scala.io.StdIn

object Fibonacci extends App {

  def calc_fib_recursive(n: Int): Long =
    if (n <= 1) n
    else calc_fib(n - 1) + calc_fib(n - 2)

  def calc_fib_big(n: Int): BigInt =
    if (n <= 1) BigInt(n)
    else {
      val array = Array.ofDim[BigInt](n + 1)
      array(0) = BigInt(0)
      array(1) = BigInt(1)
      for (i <- 2 to n)
        array(i) = array(i - 1) + array(i - 2)
      array(n)
    }

  def calc_fib(n: Int): Long =
    if (n <= 1) n
    else {
      val array = Array.ofDim[Long](n + 1)
      array(0) = 0
      array(1) = 1
      for (i <- 2 to n)
        array(i) = array(i - 1) + array(i - 2)
      array(n)
    }

  val n = StdIn.readInt
  println(calc_fib(n))
}
