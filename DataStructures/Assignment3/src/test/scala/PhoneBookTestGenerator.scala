import java.io.{ BufferedOutputStream, File, FileInputStream, FileOutputStream }
import java.util.Scanner

import scala.collection.mutable.{ ArrayBuffer, ListBuffer }
import scala.collection.JavaConverters._

/**
  * @author sali
  */
object PhoneBookTestGenerator extends App with TestHelper {
  override val testDataFolderPath: String = ""

  private val probabilityLow = 0.15
  private val probabilityHigh = 0.85
  private val names = ListBuffer[String]()
  private val phoneNumbers = ArrayBuffer[Int]()

  private val queries = Array("add", "del", "find")

  private def getRandomBoolean(probability: Double): Boolean = math.random <= probability

  private def getQuery: String = queries(generateInt(4) - 1)

  private def generatePhoneNumber: Int = generateInt(10000000)

  private def generatePhoneBookTestInput(n: Int, testNumber: Int): Unit = {
    val data = ArrayBuffer[String]()
    val fileNamePrefix = f"$testNumber%02d"
    val inputFile = new File(fileNamePrefix).getAbsoluteFile
    val inputWriter = new PhoneBookOutputWriter(new BufferedOutputStream(new FileOutputStream(inputFile)))

    inputWriter.println(n.toString)
    for (_ <- 1 to n) {
      val query = getQuery
      val phoneNumber = generatePhoneNumber
      val line = query match {
        case "add" =>
          val name = generateString(10)
          names += name
          phoneNumbers += phoneNumber
          s"$query $phoneNumber $name"
        case "del" =>
          s"$query $getPhoneNumberToDelete"
        case "find" =>
          s"$query $getPhoneNumberToFind"
      }
      data += line
      inputWriter.println(line)
    }

    inputWriter.flush()
    inputWriter.close()

    val phoneBook = new NaivePhoneBook()
    val inputStream = new FileInputStream(inputFile)
    val result = phoneBook.processQueries(new Scanner(inputStream)).asScala

    val outputFile = new File(s"$fileNamePrefix.a").getAbsoluteFile
    val outputWriter = new PhoneBookOutputWriter(new BufferedOutputStream(new FileOutputStream(outputFile)))
    result.foreach(outputWriter.println)

    inputStream.close()
    outputWriter.flush()
    outputWriter.close()
  }

  private def getPhoneNumber(probability: Double): Int =
    if (getRandomBoolean(probability) && phoneNumbers.length >= 2)
      phoneNumbers(generateInt(phoneNumbers.length) - 1)
    else generatePhoneNumber

  private def getPhoneNumberToDelete: Int = getPhoneNumber(probabilityLow)

  private def getPhoneNumberToFind: Int = getPhoneNumber(probabilityHigh)

  generatePhoneBookTestInput(100, 3)
}
