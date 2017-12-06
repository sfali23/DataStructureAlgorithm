import java.io.{ BufferedOutputStream, OutputStream, PrintWriter }
import java.util.Scanner

/**
  * @author sali
  */
trait PhoneBookWork {

  private val tableSize = 10000000
  private val phoneBook = Array.ofDim[String](tableSize)

  def processQueries(scanner: Scanner, writer: OutputWriter): Unit = {
    val query = scanner.next()
    val number = scanner.nextInt()
    query match {
      case "add" =>
        val name = scanner.next()
        phoneBook(number) = name
      case "del" =>
        phoneBook(number) = null
      case "find" =>
        val name = phoneBook(number)
        writer.println(if (name == null) "not found" else name)
      case _ => // ignore, do nothing
    }
  }
}

class OutputWriter(stream: OutputStream = System.out) {
  private val writer = new PrintWriter(new BufferedOutputStream(stream))

  def flush(): Unit = writer.flush()

  def println(value: String): Unit = writer.println(value)
}

object PhoneBook extends App with PhoneBookWork {
  val scanner = new Scanner(System.in)
  val n = scanner.nextInt()
  val writer = new OutputWriter()
  for (_ <- 1 to n) processQueries(scanner, writer)
  writer.flush()
}
