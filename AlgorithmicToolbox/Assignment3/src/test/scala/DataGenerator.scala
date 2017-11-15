import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.util.Random

/**
  * @author sali
  */
trait DataGenerator {

  private val rand = new Random()

  def generateInt(start: Int, end: Int): Int = start + rand.nextInt(end - start)

  def generateInt(end: Int): Int = generateInt(1, end)

  def generatePoints(n: Int, start: Int, end: Int): Array[Int] = {
    val set = mutable.Set[Int]()
    while (set.size != n) set += generateInt(start, end)
    set.toArray
  }

  def generatePairs(n: Int, start: Int, end: Int): Array[(Long, Long)] = {
    val set = mutable.Set[Long]()
    while (set.size != n) set += generateInt(start, end)
    (for {
      x <- set
      y = generateInt(x.toInt, end).toLong
    } yield (x, y)).toArray
  }

  def printTimeElapsed(startTime: Long, description: String): Unit = {
    val endTime = System.nanoTime()
    val seconds = TimeUnit.NANOSECONDS.toSeconds(endTime - startTime)
    val millis = TimeUnit.NANOSECONDS.toMillis(endTime - startTime) % 1000
    println(s"$description: $seconds seconds $millis ms")
  }
}
