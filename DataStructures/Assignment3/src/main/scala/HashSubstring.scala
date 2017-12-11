import java.io.{BufferedOutputStream, OutputStream, PrintWriter}
import java.util.Scanner

/**
  * @author sali
  */
trait HashSubstringWork {

  private val prime = 1000000007
  private val multiplier = 239

  protected def polyHash(s: String): Int = {
    var h = 0D
    for (i <- s.length - 1 to 0 by -1) {
      h = mod(h * multiplier + s.charAt(i))
    }
    h.toInt
  }

  protected def preComputeHashes(text: String, pattern: String): Array[Int] = {
    val sourceLen = text.length
    val patternLen = pattern.length
    val last = sourceLen - patternLen
    val hashes = Array.ofDim[Int](last + 1)
    val s = text.substring(last, sourceLen)
    hashes(last) = polyHash(s)
    var y = 1D
    for (_ <- 1 to patternLen) {
      y = mod(y * multiplier)
    }

    val upper = last - 1
    for (i <- upper to 0 by -1) {
      val a = mod(mod(multiplier) * mod(hashes(i + 1)))
      val b = mod(text.charAt(i))
      val c = mod(mod(y) * mod(text.charAt(i + patternLen)))
      hashes(i) = mod(a + b - c).toInt
    }

    hashes
  }

  private def mod(value: Double) = ((value % prime) + prime) % prime

  def search(scanner: Scanner, writer: HashSubstringOutputWriter): Unit = {
    val (pattern, text) = (scanner.nextLine(), scanner.nextLine())
    val patternHash = polyHash(pattern)
    val hashes = preComputeHashes(text, pattern)

    val result = (for {
      i <- hashes.indices
      if hashes(i) == patternHash
    } yield i).mkString(" ")
    writer.println(result)
    writer.flush()
  }

}

class HashSubstringOutputWriter(stream: OutputStream = System.out) {
  private val writer = new PrintWriter(new BufferedOutputStream(stream))

  def flush(): Unit = writer.flush()

  def close(): Unit = writer.close()

  def println(value: String = " "): Unit = writer.println(value)
}

object HashSubstring extends App with HashSubstringWork {
  val scanner = new Scanner(System.in)
  search(scanner, new HashSubstringOutputWriter())
}
