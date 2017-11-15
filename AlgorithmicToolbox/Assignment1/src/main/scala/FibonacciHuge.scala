import scala.io.StdIn

/**
  * @author sali
  */
object FibonacciHuge extends App {

  private def matrixMult(m1: Array[BigInt], m2: Array[BigInt]): Array[BigInt] =
    if (m2.length == 2) {
      val a = m1(0) * m2(0) + m1(1) * m2(1)
      val b = m1(2) * m2(0) + m1(3) * m2(1)
      Array(a, b)
    } else {
      val a = m1(0) * m2(0) + m1(1) * m2(2)
      val b = m1(0) * m2(1) + m1(1) * m2(3)
      val c = m1(2) * m2(0) + m1(3) * m2(2)
      val d = m1(2) * m2(1) + m1(3) * m2(3)
      Array(a, b, c, d)
    }

  private def power(x: Array[BigInt], n: Long): Array[BigInt] = {
    val m = math.floor(n.toDouble / 2).toLong
    if (n == 1) x
    else if (n % 2 == 0) power(matrixMult(x, x), m)
    else matrixMult(x, power(matrixMult(x, x), m))
  }

  def fibonacci(n: Long): BigInt = {
    val a = Array(BigInt(1), BigInt(1), BigInt(1), BigInt(0))
    val v = Array(BigInt(1), BigInt(0))
    if (n <= 1) BigInt(n)
    else matrixMult(power(a, n - 1), v)(0)
  }

  def getFibonacciHuge2(n: Long, m: Long): Long = (fibonacci(n) % m).toLong

  def getFibonacciHuge(n: Long, m: Long): Long = {
    def f(n: BigInt): (BigInt, BigInt) =
      if (n.equals(BigInt(0))) (BigInt(0), BigInt(1))
      else {
        val two = BigInt(2)
        val m1 = BigInt(m)
        val n1 = BigDecimal(n.toLong) / BigDecimal(2)
        // println(n1)
        val (a, b) = f(BigInt(n1.doubleValue().floor.toLong))
        val c = (a        % m1) * (((two % m1) * (b % m1)) - (a % m1))
        val d = (a.pow(2) % m1) + (b.pow(2) % m1)
        if (n             % two == 0) {
          // println("1")
          (c, d)
        } else {
          // println("2")
          (d, (c + d) % m1)
        }
      }

    val r = f(BigInt(n))
    println(r)
    r._1.toLong
  }

  private val line = StdIn.readLine
  private val arr: Array[String] = line.split(" ")
  private val n = arr(0).toLong
  private val m = arr(1).toLong
  println(getFibonacciHuge(n, m))
}
