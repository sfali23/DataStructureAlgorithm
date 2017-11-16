import scala.io.StdIn

trait PrimitiveCalculatorWork {

  implicit class ComparisonUtil(tuple: (Int, Int)) {
    def min(other: (Int, Int)): (Int, Int) =
      if (tuple._2 <= other._2) tuple else other
  }

  def optimalSequence(n: Int): Array[Int] = {
    def ops1(n: Int, table: Array[(Int, Int)]) = (n - 1, table(n - 1)._2)

    def ops2(n: Int, table: Array[(Int, Int)]) =
      if (n % 2 == 0) (n / 2, table(n / 2)._2) else (n / 2, Int.MaxValue)

    def ops3(n: Int, table: Array[(Int, Int)]) =
      if (n % 3 == 0) (n / 3, table(n / 3)._2) else (n / 3, Int.MaxValue)

    if (n == 1) Array(1)
    else {
      val table: Array[(Int, Int)] = Array.ofDim[(Int, Int)](n + 1)
      table(0) = (0, 0)
      table(1) = (0, 0)

      for (num <- 2 to n) {
        val (index, op) = ops3(num, table)
          .min(ops2(num, table))
          .min(ops1(num, table))
        table(num) = (index, 1 + op)
      }

      reConstructSolution(table)
    }
  }

  private def reConstructSolution(table: Array[(Int, Int)]): Array[Int] = {
    var current = table.last
    val result = Array.ofDim[Int](current._2 + 1)
    result(0) = 1
    result(result.length - 1) = table.length - 1
    var tableIndex = current._1
    var index = result.length - 2
    while (current._1 >= 1) {
      result(index) = tableIndex
      current = table(current._1)
      index -= 1
      tableIndex = current._1
    }
    result
  }

}
object PrimitiveCalculator extends App with PrimitiveCalculatorWork {
  val n = StdIn.readInt()
  val seq = optimalSequence(n)
  println(seq.length - 1)
  println(seq.mkString(" "))
}
