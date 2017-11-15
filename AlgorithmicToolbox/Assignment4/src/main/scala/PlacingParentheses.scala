import scala.io.StdIn

trait PlacingParenthesesWork {

  def getMaximumValue(expr: String): Long = {
    val n = expr.length / 2 + 1 // number of digits
    val (minTable, maxTable) = initializeArrays(expr, n)

    for (s <- 1 until n) {
      for (i <- 0 until n - s) {
        val j = i + s
        val (min, max) = minAndMax(i, j, expr, maxTable, minTable)
        maxTable(i)(j) = max
        minTable(i)(j) = min
      }
    }

    maxTable(0)(n - 1)
  }

  private def initializeArrays(expr: String, n: Int) = {
    val minTable = Array.ofDim[Long](n, n)
    val maxTable = Array.ofDim[Long](n, n)
    for (i <- minTable.indices) {
      for (j <- minTable(0).indices) {
        if (i == j) {
          val value = expr.charAt(2 * i).toInt - 48
          minTable(i)(j) = value
          maxTable(i)(j) = value
        }
      }
    }
    (minTable, maxTable)
  }

  private def minAndMax(i: Int,
                        j: Int,
                        expr: String,
                        maxTable: Array[Array[Long]],
                        minTable: Array[Array[Long]]): (Long, Long) = {
    var _min = Double.PositiveInfinity
    var _max = Double.NegativeInfinity

    for (k <- i until j) {
      val op = expr.charAt(k * 2 + 1)
      val a = eval(maxTable(i)(k), maxTable(k + 1)(j), op)
      val b = eval(maxTable(i)(k), minTable(k + 1)(j), op)
      val c = eval(minTable(i)(k), maxTable(k + 1)(j), op)
      val d = eval(minTable(i)(k), minTable(k + 1)(j), op)
      _min = _min.min(a).min(b).min(c).min(d)
      _max = _max.max(a).max(b).max(c).max(d)
    }

    (_min.toLong, _max.toLong)
  }

  private def printArray(table: Array[Array[Long]]): Unit = {
    table.foreach(arr => println(arr.mkString(" ")))
    println()
  }

  private def eval(a: Long, b: Long, op: Char): Long =
    Option(op) match {
      case Some('+') => a + b
      case Some('-') => a - b
      case Some('*') => a * b
      case Some(_) => 0L
      case None => 0L
    }

}

object PlacingParentheses extends App with PlacingParenthesesWork {
  println(getMaximumValue(StdIn.readLine()))
}
