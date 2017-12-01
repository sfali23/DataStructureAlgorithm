import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

/**
  * @author sali
  */
@RunWith(classOf[JUnitRunner])
class JobQueueTest extends FunSuite with BeforeAndAfterAll with JobQueueWork with TestHelper {

  test("build heap") {
    val heap = JobQueueHeap(2)
    heap.insert(JobInfo(0))
    heap.insert(JobInfo(1))
    println("After Insert")
    heap.print()
    println()

    var jobInfo = heap.get
    heap.insert(JobInfo(jobInfo.workerId, 1))
    jobInfo = heap.get
    heap.insert(JobInfo(jobInfo.workerId, 2))
    println(s"After: get and insert twice")
    heap.print()
    println()

    jobInfo = heap.get
    heap.insert(JobInfo(jobInfo.workerId, 3))
    println(s"After: get and insert once")
    heap.print()
    println()

    jobInfo = heap.get
    heap.insert(JobInfo(jobInfo.workerId, 4))
    println(s"After: get and insert once")
    heap.print()
    println()

    jobInfo = heap.get
    heap.insert(JobInfo(jobInfo.workerId, 5))
    println(s"After: get and insert once")
    heap.print()
    println()

    /*heap.insertAll(42, 29, 18, 14, 7, 18, 12, 11, 13)
    assert(heap.heap === Array(7, 11, 12, 13, 18, 29, 18, 42, 14))*/
  }

  test("JobQueue: predefined tests"){
    for (i <- 1 to 4) {
      val fileNamePrefix = f"$i%02d"
      println(s"Running test: $fileNamePrefix")
      val scanner = new Scanner(getResourceAsStream(fileNamePrefix))
      val (pq, jobs) = readData(scanner)

      val result = assignJobs(pq, jobs).toList
      val output = readLines(fileNamePrefix, Some("a"))
      assert(output === result)
    }
  }
  override val testDataFolderPath: String = "job-queue"
}
