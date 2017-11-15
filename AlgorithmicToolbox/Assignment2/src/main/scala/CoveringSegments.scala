trait CoveringSegmentsImpl {

  def optimalPoints(segments: Array[(BigInt, BigInt)]): Int = {
    val sortedSegments = segments.sortWith {
      case ((a1, b1), (a2, b2)) => b1 < b2
    }
    println(sortedSegments.mkString("[", ",", "]"))
    0
  }
}

object CoveringSegments extends App with CoveringSegmentsImpl {

}
