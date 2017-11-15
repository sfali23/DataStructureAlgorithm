import scala.io.StdIn

trait DotProductImpl {

  def maxDotProduct(a: Array[BigInt], b: Array[BigInt]): BigInt = {
    val sortedA = a.sortWith(_ > _)
    val sortedB = b.sortWith(_ > _)
    sortedA.zip(sortedB).foldLeft(BigInt(0)) {
      case (prodSum, (a1, b1)) => prodSum + (a1 * b1)
    }
  }

  def readData: (Array[BigInt], Array[BigInt]) = {
    val n = StdIn.readInt()
    (readArray(n), readArray(n))
  }

  private def readArray(n: Int): Array[BigInt] = {
    val result = Array.ofDim[BigInt](n)
    val line = StdIn.readLine()
    val splits = line.split(" ")
    for (i <- splits.indices) {
      result(i) = BigInt(splits(i))
    }
    result
  }
}

object DotProduct extends App with DotProductImpl {
  val (a, b) = readData
  println(maxDotProduct(a, b))
}
