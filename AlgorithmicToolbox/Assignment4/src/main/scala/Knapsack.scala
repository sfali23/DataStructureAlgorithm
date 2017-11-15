import scala.io.StdIn

trait KnapsackWork {

  def optimalWeight(totalWeight: Int,
                    weightsAndValues: Array[(Int, Int)]): Int = {
    val rows = weightsAndValues.length + 1
    val columns = totalWeight + 1
    val values = Array.ofDim[Int](rows, columns)

    for (row <- 1 to weightsAndValues.length) {
      for (col <- 1 to totalWeight) {
        val currentWeight = weightsAndValues(row - 1)._1
        val currentValue = weightsAndValues(row - 1)._2
        if (currentWeight > col) {
          values(row)(col) = values(row - 1)(col)
        } else {
          val valueExcludingCurrentWeight = values(row - 1)(col)
          val remainingValue = values(row - 1)(col - currentWeight)
          val valueIncludingCurrentWeight = currentValue + remainingValue
          values(row)(col) =
            valueExcludingCurrentWeight.max(valueIncludingCurrentWeight)
        }
      }
    }

    values.last.last
  }

}

object Knapsack extends App with KnapsackWork {

  private val (totalWeight, weightsAndValues) = readData
  println(optimalWeight(totalWeight, weightsAndValues))

  private def readData: (Int, Array[(Int, Int)]) = {
    var line = StdIn.readLine()
    var split = line.split(" ")
    val totalWeight = split(0).toInt
    val weightsAndValues = Array.ofDim[(Int, Int)](split(1).toInt)

    line = StdIn.readLine()
    split = line.split(" ")
    for (i <- weightsAndValues.indices) {
      val value = split(i).toInt
      weightsAndValues(i) = (value, value)
    }
    (totalWeight, weightsAndValues)
  }
}
