import java.io.InputStream
import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
  * @author sali
  */
trait TestHelper {

  private val rand = new Random()
  val testDataFolderPath: String

  /**
    * generates Int between start (inclusive) and end (exclusive).
    *
    * @param start start value (inclusive)
    * @param end end value (exclusive)
    * @return generated Int between start (inclusive) and end (exclusive)
    */
  def generateInt(start: Int, end: Int): Int = start + rand.nextInt(end - start)

  /**
    * generates Int between 1 (inclusive) and end (exclusive).
    * @param end end value (exclusive)
    * @return generated Int between 1 (inclusive) and end (exclusive)
    */
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
    label = if (label.isEmpty) s"$timeElapsed ns" else label

    println(s"$description: $label")
  }

  def getResourcePath(pathPrefix: String, suffix: Option[String] = None): String = {
    val extension = if(suffix.nonEmpty) s".${suffix.get}" else ""
    s"/$testDataFolderPath/$pathPrefix$extension"
  }

  def getResourceAsStream(pathPrefix: String, suffix: Option[String] = None): InputStream =
    getClass.getResourceAsStream(getResourcePath(pathPrefix, suffix))

  def readLines(pathPrefix: String, suffix: Option[String] = None): List[String] =
    readLines(getResourcePath(pathPrefix, suffix))

  def readLines(resourcePath: String): List[String] =
    Source
      .fromInputStream(getClass.getResourceAsStream(resourcePath))
      .getLines()
      .toList
}
