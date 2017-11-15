/**
  * @author sali
  */
class Matrix(val rows: Int, val columns: Int, xs: BigInt*) {
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

  def +=(x: Int, y: Int, value: BigInt): Matrix = {
    matrix(getIndex(x, y)) = value
    this
  }

  def +=(x: Int, y: Int, value: Long): Matrix = this += (x, y, BigInt(value))

  def *(other: Matrix): Matrix = {
    assert(this.columns == other.rows)
    val result = Matrix(this.rows, other.columns)

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

  def %*(other: Matrix, mod: BigInt): Matrix = {
    assert(this.columns == other.rows)
    val result = Matrix(this.rows, other.columns)

    for (x <- 0 until this.rows) {
      for (y <- 0 until other.columns) {
        val thisRow = getRow(x)
        val otherColumn = other.getColumn(y)
        val value = thisRow
          .zip(otherColumn)
          .map {
            case (a, b) => ((a % mod) * (b % mod)) % mod
          }
          .sum
        result += (x, y, value)
      }
    }
    result
  }

  def ^(exponent: BigInt): Matrix = {
    import scala.util.control.Breaks._

    var prod = Matrix.identityMatrix(this.rows)
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

  def %^(exponent: BigInt, mod: BigInt): Matrix = {
    import scala.util.control.Breaks._

    var prod = Matrix.identityMatrix(this.rows)
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

object Matrix {

  def apply(rows: Int, columns: Int, xs: BigInt*): Matrix = new Matrix(rows, columns, xs: _*)

  def identityMatrix(size: Int): Matrix = {
    val result = Matrix(size, size)
    for (x <- 0 until result.rows) {
      for (y <- 0 until result.columns) {
        if (x == y) result += (x, y, 1)
      }
    }
    result
  }

}
