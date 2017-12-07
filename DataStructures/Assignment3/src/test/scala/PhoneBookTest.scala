import java.io.ByteArrayOutputStream
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class PhoneBookTest extends FunSuite with PhoneBookWork with TestHelper {
  override val testDataFolderPath: String = "phone-book"

  test("pre-defined tests") {
    val tests = Array(1, 2, 3)
    for (i <- tests) {
      val fileNamePrefix = f"$i%02d"
      val scanner = new Scanner(getResourceAsStream(fileNamePrefix))
      val byteArrayOutputStream = new ByteArrayOutputStream()
      val writer = new PhoneBookOutputWriter(byteArrayOutputStream)
      processQueries(scanner, writer)

      val output = readLines(fileNamePrefix, Some("a")).mkString(System.lineSeparator()) + System.lineSeparator()
      assert(output === new String(byteArrayOutputStream.toByteArray))
      byteArrayOutputStream.close()
    }
  }
}
