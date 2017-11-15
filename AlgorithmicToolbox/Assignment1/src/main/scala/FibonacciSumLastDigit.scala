import scala.io.StdIn

class FibonacciSumLastDigitMatrix(val rows: Int, val columns: Int, xs: BigInt*) {
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

  def +=(x: Int, y: Int, value: BigInt): FibonacciSumLastDigitMatrix = {
    matrix(getIndex(x, y)) = value
    this
  }

  def +=(x: Int, y: Int, value: Long): FibonacciSumLastDigitMatrix = this += (x, y, BigInt(value))

  def %*(other: FibonacciSumLastDigitMatrix, mod: BigInt): FibonacciSumLastDigitMatrix = {
    assert(this.columns == other.rows)
    val result = FibonacciSumLastDigitMatrix(this.rows, other.columns)

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

  def %^(exponent: BigInt, mod: BigInt): FibonacciSumLastDigitMatrix = {
    import scala.util.control.Breaks._

    var prod = FibonacciSumLastDigitMatrix.identityMatrix(this.rows)
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

object FibonacciSumLastDigitMatrix {

  private val baseMatrix = FibonacciSumLastDigitMatrix(2, 1, 1, 0)
  private val qMatrix = FibonacciSumLastDigitMatrix(2, 2, 1, 1, 1, 0)

  def apply(rows: Int, columns: Int, xs: BigInt*): FibonacciSumLastDigitMatrix =
    new FibonacciSumLastDigitMatrix(rows, columns, xs: _*)

  def identityMatrix(size: Int): FibonacciSumLastDigitMatrix = {
    val result = FibonacciSumLastDigitMatrix(size, size)
    for (x <- 0 until result.rows) {
      for (y <- 0 until result.columns) {
        if (x == y) result += (x, y, 1)
      }
    }
    result
  }

  def fibonacciSumLastDigit(n: BigInt): BigInt =
    if (n <= BigInt(1)) n
    else {
      val matrix1 = qMatrix %^ (n + 1, 10)
      val value = (matrix1 %* (baseMatrix, 10))(0, 0)
      if (value == 0) 9 else (value - 1) % 10
    }

}

object FibonacciSumLastDigit extends App {

  val n = StdIn.readLine()
  println(FibonacciSumLastDigitMatrix.fibonacciSumLastDigit(BigInt(n)))

}
