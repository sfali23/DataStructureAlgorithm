import scala.io.StdIn

/**
  * @author sali
  */
object LCM extends App {

  private def gcdInternal(a: Long, b: Long): Long =
    if (b <= 0) a
    else gcdInternal(b, a % b)

  def lcm(a: Long, b: Long): Long =
    (a * b) / gcdInternal(a, b)

  private val line = StdIn.readLine
  private val arr: Array[String] = line.split(" ")
  private val a = arr(0).toInt
  private val b = arr(1).toInt
  println(lcm(a, b))
}
