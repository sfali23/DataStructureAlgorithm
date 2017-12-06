import java.io.{ BufferedOutputStream, OutputStream, PrintWriter }
import java.util.Scanner

/**
  * @author sali
  */
trait PhoneBookWork {

  val addQuery: String = "add"
  val delQuery: String = "del"
  val findQuery: String = "find"
  private val tableSize = 10000000
  private val phoneBook = Array.ofDim[String](tableSize)

  def processQuery(scanner: Scanner, writer: OutputWriter): Unit = {
    val query = scanner.next()
    val number = scanner.nextInt()
    query match {
      case `addQuery` =>
        val name = scanner.next()
        phoneBook(number) = name
      case `delQuery` =>
        phoneBook(number) = null
      case `findQuery` =>
        val name = phoneBook(number)
        writer.println(if (name == null) "not found" else name)
      case _ => // ignore, do nothing
    }
  }

  def processQueries(scanner: Scanner, writer: OutputWriter): Unit = {
    val n = scanner.nextInt()
    for (_ <- 1 to n) processQuery(scanner, writer)
    writer.flush()
  }
}

class OutputWriter(stream: OutputStream = System.out) {
  private val writer = new PrintWriter(new BufferedOutputStream(stream))

  def flush(): Unit = writer.flush()

  def println(value: String): Unit = writer.println(value)
}

object PhoneBook extends App with PhoneBookWork {
  processQueries(new Scanner(System.in), new OutputWriter())
}
