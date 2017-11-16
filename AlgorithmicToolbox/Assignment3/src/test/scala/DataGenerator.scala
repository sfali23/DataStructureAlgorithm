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
    val timeElapsed = endTime - startTime

    var label = ""

    var unit = TimeUnit.NANOSECONDS.toMillis(timeElapsed)
    val millis = if (unit > 1000) unit % 1000 else unit
    label = if (millis > 0) s"$millis ms" else label

    unit = TimeUnit.NANOSECONDS.toSeconds(timeElapsed)
    val seconds = if (unit > 60) unit % 60 else unit
    label = if (seconds > 0) s"$seconds seconds " + label else label

    unit = TimeUnit.NANOSECONDS.toMinutes(timeElapsed)
    val minutes = if (unit > 60) unit % 60 else unit
    label = if (minutes > 0) s"$minutes minutes " + label else label
    label = if(label.isEmpty) s"$timeElapsed ns" else label

    println(s"$description: $label")
  }
}
