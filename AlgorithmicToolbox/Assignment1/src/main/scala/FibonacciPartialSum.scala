import scala.io.StdIn

class FibonacciPartialSumMatrix(val rows: Int, val columns: Int, xs: BigInt*) {
  protected val matrix: Array[BigInt] = new Array[BigInt](rows * columns)

  if (xs.nonEmpty) {
    assert(xs.length == rows * columns)
    for (x <- 0 until rows) {
      for (y <- 0 until columns) {
        this += (x, y, xs(getIndex(x, y)))
      }
    }
  } else {
    for (x <- 0 until rows) {
      for (y <- 0 until columns) {
        this += (x, y, 0)
      }
    }
  }

  def apply(x: Int, y: Int): BigInt = {
    assert(x < this.rows, s"value of `x` must between 0 and ${rows - 1}, instead got $x")
    assert(y < this.columns, s"value of `y` must between 0 and ${columns - 1}, instead got $y")
    matrix(getIndex(x, y))
  }

  def +=(x: Int, y: Int, value: BigInt): FibonacciPartialSumMatrix = {
    matrix(getIndex(x, y)) = value
    this
  }

  def +=(x: Int, y: Int, value: Long): FibonacciPartialSumMatrix = this += (x, y, BigInt(value))

  def ^(exponent: BigInt): FibonacciPartialSumMatrix = {
    import scala.util.control.Breaks._

    var prod = FibonacciPartialSumMatrix.identityMatrix(this.rows)
    var exp = exponent
    var base = this
    breakable {
      while (true) {
        if ((exp & BigInt(1)) == BigInt(1)) {
          prod = prod * base
        }
        exp /= BigInt(2)
        if (exp <= BigInt(0)) {
          break
        }
        base = base * base
      }
    }
    prod
  }

  def *(other: FibonacciPartialSumMatrix): FibonacciPartialSumMatrix = {
    assert(this.columns == other.rows)
    val result = FibonacciPartialSumMatrix(this.rows, other.columns)

    for (x <- 0 until this.rows) {
      for (y <- 0 until other.columns) {
        val thisRow = getRow(x)
        val otherColumn = other.getColumn(y)
        val value = thisRow
          .zip(otherColumn)
          .map {
            case (a, b) => a * b
          }
          .sum
        result += (x, y, value)
      }
    }
    result
  }

  def %*(other: FibonacciPartialSumMatrix, mod: BigInt): FibonacciPartialSumMatrix = {
    assert(this.columns == other.rows)
    val result = FibonacciPartialSumMatrix(this.rows, other.columns)

    for (x <- 0 until this.rows) {
      for (y <- 0 until other.columns) {
        val thisRow = getRow(x)
        val otherColumn = other.getColumn(y)
        val value = thisRow
          .zip(otherColumn)
          .map {
            case (a, b) => (a % mod) * (b % mod)
          }
          .sum
        result += (x, y, value)
      }
    }
    result
  }

  def %^(exponent: BigInt, mod: BigInt): FibonacciPartialSumMatrix = {
    import scala.util.control.Breaks._

    var prod = FibonacciPartialSumMatrix.identityMatrix(this.rows)
    var exp = exponent
    var base = this
    breakable {
      while (true) {
        if ((exp & BigInt(1)) == BigInt(1)) {
          prod = prod %* (base, mod)
        }
        exp /= BigInt(2)
        if (exp <= BigInt(0)) {
          break
        }
        base = base %* (base, mod)
      }
    }
    prod
  }

  private def getIndex(x: Int, y: Int): Int = y * this.rows + x

  private def getRow(row: Int): Array[BigInt] =
    (for {
      y <- 0 until this.columns
      value = this(row, y)
    } yield value).toArray

  private def getColumn(column: Int): Array[BigInt] =
    (for {
      x <- 0 until this.rows
      value = this(x, column)
    } yield value).toArray

  override def toString: String = {
    val builder: StringBuilder = new StringBuilder()
    for (x <- 0 until rows) {
      for (y <- 0 until columns) {
        builder.append(this(x, y)).append(" ")
      }
      builder.append(System.lineSeparator())
    }
    builder.toString()
  }
}

object FibonacciPartialSumMatrix {

  private val baseMatrix = FibonacciPartialSumMatrix(2, 1, 1, 0)
  private val qMatrix = FibonacciPartialSumMatrix(2, 2, 1, 1, 1, 0)

  def apply(rows: Int, columns: Int, xs: BigInt*): FibonacciPartialSumMatrix =
    new FibonacciPartialSumMatrix(rows, columns, xs: _*)

  private def identityMatrix(size: Int): FibonacciPartialSumMatrix = {
    val result = FibonacciPartialSumMatrix(size, size)
    for (x <- 0 until result.rows) {
      for (y <- 0 until result.columns) {
        if (x == y) result += (x, y, 1)
      }
    }
    result
  }

  private def fibonacciNumber(n: BigInt): BigInt =
    if (n <= 1) n
    else ((qMatrix ^ (n - 1)) * baseMatrix)(0, 0)

  private def fibonacciNumberModLastDigit(n: BigInt, mod: BigInt): BigInt =
    ((qMatrix %^ (n - 1, mod)) %* (baseMatrix, mod))(0, 0)

  private def fibonacciNumberLastDigit(n: BigInt): BigInt = fibonacciNumberModLastDigit(n, 10)

  private def +%(n: BigInt, mod: BigInt): BigInt =
    if (n <= BigInt(1)) n
    else {
      val matrix1 = qMatrix %^ (n + 1, mod)
      val value = (matrix1 %* (baseMatrix, mod))(0, 0)
      if (value % mod == 0) mod - 1 else (value - 1) % mod
    }

  def fibonacciPartialSum(m: BigInt, n: BigInt): BigInt =
    if (n <= BigInt(1)) n
    else if (m == n) fibonacciNumberLastDigit(n)
    else {
      val mod = 10000
      val number = fibonacciNumberModLastDigit(m, mod)
      val sumOfM = +%(m, mod)
      val sumOfN = +%(n, mod)
      (sumOfN - sumOfM + number) % 10
    }

}

object FibonacciPartialSum extends App {
  private val line = StdIn.readLine
  private val arr: Array[String] = line.split(" ")
  private val m = arr(0)
  private val n = arr(1)
  println(FibonacciPartialSumMatrix.fibonacciPartialSum(BigInt(m), BigInt(n)))
}
