trait ProcessPackagesWork {

}

case class Request(arrivalTime: Int, processTime: Int)

case class Response(dropped: Boolean, startTime: Int)

class Buffer(size: Int){
  private var finishTime = List[Int]()

  def process(request: Request): Response = {
    Response(dropped = false, startTime = -1)
  }
}

object ProcessPackages extends App with ProcessPackagesWork {

}
