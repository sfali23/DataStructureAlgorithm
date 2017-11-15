import scala.io.StdIn
import scala.util.control.Breaks._

/**
  * @author sali
  */
trait FractionalKnapsackImpl {

  private def fractionalKnapsack(capacity: Double, valuesAndWeight: Array[(Double, Double)]): (Double, Array[Double]) = {
    val amounts = Array.ofDim[Double](valuesAndWeight.length)
    var totalValue = 0.0
    var remainingCapacity = capacity
    breakable {
      for (i <- valuesAndWeight.indices) {
        if (remainingCapacity <= 0) break
        else {
          val current = valuesAndWeight(i)
          val a = math.min(current._2, remainingCapacity)
          totalValue += a * (current._1 / current._2)
          amounts(i) = amounts(i) + a
          remainingCapacity -= a
        }
      }
    }
    (totalValue, amounts)
  }

  def getOptimalValue(capacity: Double, valuesAndWeight: Array[(Double, Double)]): Double = {
    var totalValue = 0.0
    var remainingCapacity = capacity
    breakable {
      for (i <- valuesAndWeight.indices) {
        if (remainingCapacity <= 0) break
        else {
          val current = valuesAndWeight(i)
          val a = math.min(current._2, remainingCapacity)
          totalValue += a * (current._1 / current._2)
          remainingCapacity -= a
        }
      }
    }
    totalValue
  }

  def readData: (Double, Array[(Double, Double)]) = {
    var line = StdIn.readLine()
    var args = line.split(" ")
    val n = args(0).toInt
    val capacity = args(1).toInt
    var valuesAndWeight = Array.ofDim[(Double, Double)](n)
    for (i <- 0 until n) {
      line = StdIn.readLine()
      args = line.split(" ")
      val value = args(0).toInt
      val weight = args(1).toInt
      valuesAndWeight(i) = (value, weight)
    }
    valuesAndWeight = valuesAndWeight.sortWith {
      case ((value1, weight1), (value2, weight2)) => value1 / weight1 > value2 / weight2
    }
    (capacity, valuesAndWeight)
  }
}

object FractionalKnapsack extends App with FractionalKnapsackImpl {

  val (capacity, valuesAndWeight) = readData
  private val optimalValue = getOptimalValue(capacity, valuesAndWeight)
  println(BigDecimal(optimalValue).setScale(4, BigDecimal.RoundingMode.HALF_UP).toString())
}
