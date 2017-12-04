import java.io.ByteArrayOutputStream
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class MergingTablesTest extends FunSuite with MergingTablesWork with TestHelper {
  override val testDataFolderPath: String = "merge-tables"

  test("Initial") {
    val tests = Array(1, 2, 116)
    for (i <- tests) {
      val fileNamePrefix = f"$i%02d"
      val scanner = new Scanner(getResourceAsStream(fileNamePrefix))
      val byteArrayOutputStream = new ByteArrayOutputStream()
      val writer = new OutputWriter(byteArrayOutputStream)
      run(scanner, writer)

      val output = readLines(fileNamePrefix, Some("a")).mkString(System.lineSeparator()) + System.lineSeparator()
      assert(output === new String(byteArrayOutputStream.toByteArray))
      byteArrayOutputStream.close()
    }
  }
}
