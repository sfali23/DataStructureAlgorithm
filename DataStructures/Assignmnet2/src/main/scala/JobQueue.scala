import java.util.Scanner

import scala.io.StdIn

case class JobInfo(workerId: Int, finishTime: Long = 0) {
  def <(other: JobInfo): Boolean =
    if (this.finishTime == other.finishTime) this.workerId < other.workerId
    else this.finishTime < other.finishTime

  def compare(other: JobInfo): Int =
    if (this.finishTime == other.finishTime) this.workerId.compareTo(other.workerId)
    else this.finishTime.compareTo(other.finishTime)
}

class JobQueueHeap(maxSize: Int) {

  private var size = -1
  private val storage = Array.ofDim[JobInfo](maxSize)

  def moveUp(i: Int): Unit = {
    var index = i
    while (index >= 0 && storage(index) < storage(parent(index))) {
      val parentIndex = parent(index)
      val tmp = storage(parentIndex)
      storage(parentIndex) = storage(index)
      storage(index) = tmp
      index = parentIndex
    }
  }

  def moveDown(i: Int): Unit = {
    var minIndex = i
    val l = leftChild(i)
    minIndex = if (l <= size && storage(l) < storage(minIndex)) l else minIndex

    val r = rightChild(i)
    minIndex = if (r <= size && storage(r) < storage(minIndex)) r else minIndex

    if (i != minIndex) {
      val tmp = storage(i)
      storage(i) = storage(minIndex)
      storage(minIndex) = tmp
      moveDown(minIndex)
    }
  }

  def insert(value: JobInfo): Unit =
    if (size < maxSize) {
      size += 1
      storage(size) = value
      moveUp(size)
    }

  def insertAll(values: JobInfo*): Unit = values.foreach(insert)

  def get: JobInfo = {
    val result = storage(0)
    storage(0) = storage(size)
    size -= 1
    moveDown(0)
    result
  }

  def peek: JobInfo = storage(0)

  def isEmpty: Boolean = size < 0

  def nonEmpty: Boolean = size >= 0

  def heap: Array[JobInfo] = storage.take(size + 1)

  def print(): Unit = println(heap.map(ji => (ji.workerId, ji.finishTime)).mkString("[", ", ", "]"))

  private def parent(i: Int): Int = (i - 1) / 2
  private def leftChild(i: Int) = 2 * i + 1
  private def rightChild(i: Int) = 2 * i + 2

}

object JobQueueHeap {
  def apply(maxSize: Int): JobQueueHeap = new JobQueueHeap(maxSize)
}

trait JobQueueWork {

  def readData(scanner: Scanner): (JobQueueHeap, Array[Long]) = {
    val numOfWorkers = scanner.nextInt()
    val numOfJobs = scanner.nextInt()

    val priorityQueue = new JobQueueHeap(numOfWorkers)
    (0 until numOfWorkers).foreach(i => priorityQueue.insert(JobInfo(i)))

    val jobs = Array.ofDim[Long](numOfJobs)
    for (i <- 0 until numOfJobs) jobs(i) = scanner.nextLong()

    (priorityQueue, jobs)
  }

  def readData: (JobQueueHeap, Array[Long]) = {
    var line = StdIn.readLine()
    var splits = line.split(" ")
    val numOfWorkers = splits.head.toInt
    val numOfJobs = splits.last.toInt

    val priorityQueue = new JobQueueHeap(numOfWorkers)
    (0 until numOfWorkers).foreach(i => priorityQueue.insert(JobInfo(i)))

    val jobs = Array.ofDim[Long](numOfJobs)
    line = StdIn.readLine()
    splits = line.split(" ")
    for (i <- 0 until numOfJobs) jobs(i) = splits(i).toLong

    (priorityQueue, jobs)
  }

  def assignJobs2(priorityQueue: JobQueueHeap, jobs: Array[Long]): Array[String] = {
    import scala.util.control.Breaks._

    val response = Array.ofDim[String](jobs.length)
    var currentTime = 0L
    var count = 0
    breakable {
      while (true) {
        if (count >= jobs.length) {
          break
        }

        var top = priorityQueue.peek
        while (count < jobs.length && currentTime == top.finishTime) {
          top = priorityQueue.get
          val workerId = top.workerId
          val jobDuration = jobs(count)
          priorityQueue.insert(JobInfo(workerId, jobDuration + currentTime))
          response(count) = s"$workerId $currentTime"
          top = priorityQueue.peek
          count += 1
        }

        currentTime += 1L
      } // end of while
    } // end of breakable
    response
  }

  def assignJobs(priorityQueue: JobQueueHeap, jobs: Array[Long]): Array[String] = {
    val response = Array.ofDim[String](jobs.length)
    for(i <- jobs.indices){
      val top = priorityQueue.get
      val workerId = top.workerId
      val finishTime = top.finishTime
      val nextFinishTime = jobs(i) + finishTime
      priorityQueue.insert(JobInfo(workerId, nextFinishTime))
      response(i) = s"$workerId $finishTime"
    }
    response
  }

}

/**
  * @author sali
  */
object JobQueue extends App with JobQueueWork {
  val scanner = new Scanner(System.in)
  val (priorityQueue, jobs) = readData(scanner)
  val response = assignJobs(priorityQueue, jobs)
  response.foreach(println)
}
