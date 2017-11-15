import scala.io.StdIn

/**
  * @author sali
  */
object GCD extends App {

  def gcd(a: Long, b: Long): Long =
    if (b <= 0) a
    else gcd(b, a % b)

  private val line = StdIn.readLine
  private val arr: Array[String] = line.split(" ")
  private val a = arr(0).toInt
  private val b = arr(1).toInt
  println(gcd(a, b))
}
