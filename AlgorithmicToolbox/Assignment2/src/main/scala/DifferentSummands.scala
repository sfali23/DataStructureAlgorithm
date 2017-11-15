import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait DifferentSummandsImpl {

  private val ONE = BigInt(1)
  private val TWO = BigInt(2)

  private def differentSummandsInternal(k: BigInt, l: BigInt, result: List[BigInt]): List[BigInt] = {
    if (k <= (2 * l)) result ++: (k :: Nil)
    else differentSummandsInternal(k - l, l + 1, result ::: List(l))
  }

  def differentSummands(n: BigInt): ListBuffer[BigInt] = {
    import scala.util.control.Breaks._

    val result = ListBuffer[BigInt]()
    var k = n
    var l = ONE

    breakable {
      while (true) {
        if (k <= (TWO * l)) {
          result += k
          break
        }
        result += l
        k = k - l
        l = l + ONE
      }
    }

    result
  }

}

object DifferentSummands extends App with DifferentSummandsImpl {
  val n = StdIn.readLong()
  val result = differentSummands(BigInt(n))
  println(result.size)
  println(result.mkString(" "))
}
