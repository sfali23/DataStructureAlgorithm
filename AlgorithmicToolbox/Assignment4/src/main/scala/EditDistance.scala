import scala.io.StdIn

trait EditDistanceWork {

  def editDistance(source: String, target: String): Int = {
    val width = target.length + 1
    val height = source.length + 1
    val table = Array.ofDim[Int](height, width)

    for (i <- table.indices) {
      for (j <- table(0).indices) {
        if (i == 0 && j == 0) table(i)(j) = 0
        else if (i == 0) {
          table(i)(j) = table(i)(j - 1) + 1
        } else if (j == 0) {
          table(i)(j) = table(i - 1)(j) + 1
        } else {
          val insertion = table(i)(j - 1) + 1
          val deletion = table(i - 1)(j) + 1
          val _match = table(i - 1)(j - 1)
          val mismatch = _match + 1

          val sourceChar = source.charAt(i - 1)
          val targetChar = target.charAt(j - 1)

          val _min = insertion.min(deletion)
          table(i)(j) =
            if (sourceChar == targetChar) _min.min(_match)
            else _min.min(mismatch)
        }
      }
    }

    table.last.last
  }

}

object EditDistance extends App with EditDistanceWork {
  val source = StdIn.readLine
  val target = StdIn.readLine
  println(editDistance(source, target))
}
