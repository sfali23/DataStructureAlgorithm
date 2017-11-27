import java.util
import java.util.Scanner

import scala.collection.mutable.ListBuffer

trait ProcessPackagesWork {

  def initBuffer(scanner: Scanner): Buffer = new Buffer(scanner.nextInt())

  def readRequests(scanner: Scanner): List[Request] = {
    val numOfRequests = scanner.nextInt()
    val requests = ListBuffer[Request]()
    for (_ <- 0 until numOfRequests)
      requests += Request(scanner.nextInt(), scanner.nextInt())
    requests.toList
  }

  def processRequests(buffer: Buffer,
                      requests: List[Request]): List[Response] =
    for {
      request <- requests
      response = buffer.process(request)
    } yield response

  def processRequests(buffer: Buffer, scanner: Scanner): Array[Response] = {
    val numOfRequests = scanner.nextInt()
    val responses = Array.ofDim[Response](numOfRequests)
    for (i <- responses.indices) {
      responses(i) =
        buffer.process(Request(scanner.nextInt(), scanner.nextInt()))
    }
    responses
  }
}

case class Request(arrivalTime: Int, processTime: Int)

case class Response(startTime: Int = -1)

class Buffer(size: Int) {
  private val buffer = new util.LinkedList[Int]()

  def process(request: Request): Response = {
    val currentTime = request.arrivalTime
    val processTime = request.processTime

    if (!buffer.isEmpty && currentTime >= buffer.peekFirst()) {
      // if the front most processor is already finished
      buffer.removeFirst()
    }

    if (buffer.isEmpty) {
      val finishTime = processTime + currentTime
      buffer.add(finishTime)
      Response(currentTime)
    } else {
      if (buffer.size() < size) {
        // we still have space in buffer, queue this request
        val last = buffer.peekLast()
        val finishTime = processTime + last
        buffer.add(finishTime)
        Response(last)
      } else {
        // buffer is full, drop the packet
        Response()
      }
    }
  }
}

object ProcessPackages extends App with ProcessPackagesWork {
  val scanner = new Scanner(System.in)
  val buffer = initBuffer(scanner)
  val responses = processRequests(buffer, scanner)
  responses.foreach(response => println(response.startTime))
}
