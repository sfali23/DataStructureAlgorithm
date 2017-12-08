import java.io.{ BufferedOutputStream, OutputStream, PrintWriter }
import java.util
import java.util.Scanner

/**
  * @author sali
  */
class HashChainsWork(bucketSize: Int) {
  val addQuery: String = "add"
  val delQuery: String = "del"
  val findQuery: String = "find"
  val checkQuery: String = "check"
  private val prime = 1000000007
  private val multiplier = 263
  private val table = Array.ofDim[util.List[String]](bucketSize)

  private def hash(s: String): Int = {
    var h = 0D
    for (i <- s.length - 1 to 0 by -1) {
      h = (h * multiplier + s.charAt(i)) % prime
    }
    (h % bucketSize).toInt
  }

  private def processQuery(scanner: Scanner, writer: HashChainsOutputWriter): Unit = {
    val query = scanner.next()
    query match {
      case `addQuery` => processAddQuery(scanner)
      case `delQuery` => processDelQuery(scanner)
      case `findQuery` => processFindQuery(scanner, writer)
      case `checkQuery` => processCheckQuery(scanner, writer)
      case _ => // ignore, do nothing
    }
  }

  private def processAddQuery(scanner: Scanner): Unit = {
    val name = scanner.next()
    val index = hash(name)
    var list = table(index)
    if (list == null) {
      list = new util.ArrayList[String]()
      table(index) = list
    }
    if (!list.contains(name)) {
      list.add(0, name)
    }
  }

  private def processDelQuery(scanner: Scanner): Unit = {
    val name = scanner.next()
    val index = hash(name)
    val list = table(index)
    if (list != null && list.contains(name)) {
      list.remove(name)
    }
  }

  private def processFindQuery(scanner: Scanner, writer: HashChainsOutputWriter): Unit = {
    val name = scanner.next()
    val index = hash(name)
    val list = table(index)
    val result = if (list != null && list.contains(name)) "yes" else "no"
    writer.println(result)
  }

  private def processCheckQuery(scanner: Scanner, writer: HashChainsOutputWriter): Unit = {
    val index = scanner.nextInt()
    writer.println(mkString(table(index)))
  }

  private def mkString(list: util.List[String]): String = {
    val builder = new StringBuilder
    if (list != null && !list.isEmpty) {
      builder.append(list.get(0))
      for (i <- 1 until list.size())
        builder.append(" ").append(list.get(i))
    }
    builder.toString()
  }

  def processQueries(scanner: Scanner, writer: HashChainsOutputWriter): Unit = {
    val numberOfQueries = scanner.nextInt()
    for (_ <- 1 to numberOfQueries) processQuery(scanner, writer)
    writer.flush()
  }

}

class HashChainsOutputWriter(stream: OutputStream = System.out) {
  private val writer = new PrintWriter(new BufferedOutputStream(stream))

  def flush(): Unit = writer.flush()

  def close(): Unit = writer.close()

  def println(value: String): Unit = writer.println(value)
}

object HashChains extends App {
  val scanner = new Scanner(System.in)
  val bucketSize = scanner.nextInt()
  new HashChainsWork(bucketSize).processQueries(scanner, new HashChainsOutputWriter())
}
