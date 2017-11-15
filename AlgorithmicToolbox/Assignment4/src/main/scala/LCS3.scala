import scala.io.StdIn

case class Value(value: Long,
                 length: Int,
                 index: (Int, Int),
                 referrer: Option[Value],
                 included: Boolean = false) {
  def max(other: Value): Value =
    if (this.length >= other.length) this else other
}

trait LCS3Work {

  def lcs3(seq1: Array[Long], seq2: Array[Long], seq3: Array[Long]): Long = {
    val n1 = seq1.length + 1
    val n2 = seq2.length + 1
    val n3 = seq3.length + 1
    val table = Array.ofDim[Long](n1, n2, n3)

    for (i <- 0 until n1) {
      for (j <- 0 until n2) {
        for (k <- 0 until n3) {
          if (i == 0 || j == 0 || k == 0) table(i)(j)(k) = 0
          else {
            val v1 = seq1(i - 1)
            val v2 = seq2(j - 1)
            val v3 = seq3(k - 1)
            if (v1 == v2 && v1 == v3) {
              table(i)(j)(k) = table(i - 1)(j - 1)(k - 1) + 1
            } else {
              val t1 = table(i - 1)(j)(k)
              val t2 = table(i)(j - 1)(k)
              val t3 = table(i)(j)(k - 1)
              table(i)(j)(k) = t1.max(t2).max(t3)
            }
          }
        }
      }
    }

    table.last.last.last
  }

  def lcs2(seq1: Array[Long], seq2: Array[Long]): Array[Long] = {
    val n1 = seq1.length + 1
    val n2 = seq2.length + 1
    val table = Array.ofDim[Value](n1, n2)

    for (i <- 0 until n1) {
      for (j <- 0 until n2) {
        if (i == 0 || j == 0) table(i)(j) = Value(0, 0, (i, j), None)
        else {
          val rowElement = seq1(i - 1)
          val columnElement = seq2(j - 1)
          if (rowElement == columnElement) {
            // include this item
            val diagonalItem = table(i - 1)(j - 1)
            table(i)(j) = Value(rowElement,
                                diagonalItem.length + 1,
                                (i, j),
                                Some(diagonalItem),
                                included = true)
          } else {
            val leftItem = table(i)(j - 1)
            val topItem = table(i - 1)(j)
            val maxItem = leftItem.max(topItem)
            table(i)(j) = Value(-1, maxItem.length, (i, j), Some(maxItem))
          }
        }
      }
    }

    reconstructSolution(table)
  }

  private def reconstructSolution(source: Array[Array[Value]]): Array[Long] = {
    var current = source.last.last
    val result = Array.ofDim[Long](current.length)
    var index = result.length - 1
    while (current.referrer.nonEmpty) {
      if (current.included) {
        result(index) = current.value
        index -= 1
      }
      current = current.referrer.get
    }

    result
  }

  /*private def printTable(table: Array[Array[Value]]): Unit = {
    val intArray =
      table.map(arr => arr.map(v => if (v == null) -1 else v.length))
    println(intArray.map(_.mkString(" ")).mkString(System.lineSeparator()))
  }*/

}

object LCS3 extends App with LCS3Work {

  val seq1 = readArray
  val seq2 = readArray
  val seq3 = readArray
  println(lcs3(seq1, seq2, seq3))

  private def readArray = {
    val n = StdIn.readInt()
    val line = StdIn.readLine()
    val split = line.split(" ")
    val result = Array.ofDim[Long](n)
    for (i <- 0 until n) result(i) = split(i).toLong
    result
  }
}
