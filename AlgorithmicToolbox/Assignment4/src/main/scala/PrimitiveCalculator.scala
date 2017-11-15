import scala.io.StdIn

trait PrimitiveCalculatorWork {

  def optimalSequence(n: Int): Array[Int] = {
    if (n == 1) Array(1)
    else {
      val table: Array[(Int, Int)] = Array.ofDim[(Int, Int)](n + 1)
      table(0) = (0, 0)
      table(1) = (0, 0)

      for (num <- 2 to n) {
        val (index1, op1) = (num - 1, table(num - 1)._2)
        val (index2, op2) =
          if (num % 2 == 0) (num / 2, table(num / 2)._2)
          else (num / 2, Int.MaxValue)
        val (index3, op3) =
          if (num % 3 == 0) (num / 3, table(num / 3)._2)
          else (num / 3, Int.MaxValue)
        val (index, op) =
          if (op1 < op2 && op1 < op3) (index1, op1)
          else if (op2 <= op1 && op2 < op3) (index2, op2)
          else (index3, op3)
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
