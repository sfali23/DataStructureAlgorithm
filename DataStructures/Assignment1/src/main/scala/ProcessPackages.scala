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
}

case class Request(arrivalTime: Int, processTime: Int)

case class Response(startTime: Int = -1)

class Buffer(size: Int) {
  private var buffer = List[Int]()

  def process(request: Request): Response = {
    val currentTime = request.arrivalTime
    val processTime = request.processTime

    if (buffer.nonEmpty && currentTime >= buffer.head) {
      // if the front most processor is already finished
      buffer = buffer.tail
    }

    if (buffer.isEmpty) {
      val finishTime = processTime + currentTime
      buffer = buffer :+ finishTime
      Response(currentTime)
    } else {
      if (buffer.length < size) {
        // we still have space in buffer, queue this request
        val last = buffer.last
        val finishTime = processTime + last
        buffer = buffer :+ finishTime
        Response(last)
      } else {
        // buffer is full, drop the packet
        Response()
      }
    }
  }
}

object ProcessPackages extends App with ProcessPackagesWork {}
