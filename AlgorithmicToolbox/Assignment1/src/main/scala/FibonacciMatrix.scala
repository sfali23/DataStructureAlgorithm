/**
  * @author sali
  */
object FibonacciMatrix {
  private val baseMatrix = Matrix(2, 1, 1, 0)
  private val qMatrix = Matrix(2, 2, 1, 1, 1, 0)

  def fibonacciNumber(n: Int): BigInt = fibonacciNumber(BigInt(n))

  def fibonacciNumber(n: BigInt): BigInt =
    if (n <= 1) n
    else ((qMatrix ^ (n - 1)) * baseMatrix)(0, 0)

  def fibonacciNumberSum(n: BigInt): BigInt =
    if (n <= 1) n
    else ((qMatrix ^ (n + BigInt(1))) * baseMatrix)(0, 0) - 1

  def +%(n: BigInt, mod: BigInt): BigInt =
    if (n <= 1) n
    else {
      val value = ((qMatrix %^ (n + 1, mod)) %* (baseMatrix, mod))(0, 0)
      println(s":::: $value")
      if (value % mod == 0) value - 1 else (value - 1) % mod
    }

  def fibonacciNumberModLastDigit(n: BigInt, mod: BigInt): BigInt =
    ((qMatrix %^ (n - 1, mod)) %* (baseMatrix, mod))(0, 0)

  def fibonacciNumberLastDigit(n: BigInt): BigInt = fibonacciNumberModLastDigit(n, 10)

  def fibonacciNumberSumLastDigit(n: BigInt): BigInt = {
    val value = ((qMatrix %^ (n + BigInt(1), BigInt(10))) %* (baseMatrix, BigInt(10)))(0, 0)
    if (value == 0) 9 else (value - 1) % 10
  }

}
