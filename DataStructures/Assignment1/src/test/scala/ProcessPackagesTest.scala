import java.io.{BufferedInputStream, FileInputStream}
import java.nio.file.Paths
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class ProcessPackagesTest
    extends FunSuite
    with BeforeAndAfterAll
    with ProcessPackagesWork
    with TestHelper {

  private var startTime: Long = 0L

  override protected def beforeAll(): Unit = {
    startTime = System.nanoTime()
  }

  override protected def afterAll(): Unit = {
    printTimeElapsed(startTime, "time to run all tests")
  }

  test("ProcessPackages: Test case #1") {
    val start = 1
    val end = 24
    for (i <- start to end) {
      val fileNamePrefix = f"$i%02d"
      val path = Paths
        .get(getClass.getResource(s"/process-packages/$fileNamePrefix").toURI)
        .toAbsolutePath
      System.setIn(new BufferedInputStream(new FileInputStream(path.toFile)))
      val scanner = new Scanner(System.in)
      val buffer = initBuffer(scanner)
      val responses = processRequests(buffer, scanner).map(_.startTime)
      val output =
        readLines(s"/process-packages/$fileNamePrefix.a").map(_.toInt)
      assert(responses === output, s"'Failed test $fileNamePrefix'")
    }
  }
}
