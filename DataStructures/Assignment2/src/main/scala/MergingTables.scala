import java.io.{BufferedOutputStream, OutputStream, PrintWriter}
import java.util
import java.util.Scanner

/**
  * @author sali
  */
class TableDisJointSet(size: Int, numberOfOperations: Int) {
  private val maxSize = size + 1
  private val parents: Array[Int] = Array.ofDim[Int](maxSize)
  private val ranks: Array[Int] = Array.ofDim[Int](maxSize)
  private val numberOfRows: Array[Long] = Array.ofDim[Long](maxSize)
  private var _maximumNumberOfRows: Long = -1

  def makeSet(index: Int, rows: Long): Unit = {
    parents(index) = index
    ranks(index) = 0
    numberOfRows(index) = rows
    _maximumNumberOfRows = _maximumNumberOfRows.max(rows)
  }

  def getRank(index: Int): Int = ranks(index)

  def getParent(index: Int): Int = {
    val stack = new util.LinkedList[Int]()
    var current = index
    var parent = parents(current)
    while (current != parent) {
      stack.addFirst(current)
      current = parent
      parent = parents(current)
    }
    while (!stack.isEmpty) parents(stack.removeFirst()) = parent
    parent
  }

  def merge(destination: Int, source: Int): Unit = {
    val realDestination = getParent(destination)
    val realSource = getParent(source)
    if (realDestination != realSource) {
      val destinationRank = getRank(realDestination)
      val sourceRank = getRank(realSource)
      val totalRows = numberOfRows(realSource) + numberOfRows(realDestination)
      if (destinationRank > sourceRank) {
        parents(realDestination) = realSource
        numberOfRows(realSource) = totalRows
        numberOfRows(realDestination) = 0
      } else {
        parents(realSource) = realDestination
        numberOfRows(realDestination) = totalRows
        numberOfRows(realSource) = 0
        if (sourceRank == destinationRank)
          ranks(realDestination) = sourceRank + 1
      }
      _maximumNumberOfRows = _maximumNumberOfRows.max(totalRows)
    }
  }

  def setNumberOfRows(index: Int, rows: Long): Unit = numberOfRows(index) = rows

  def maximumNumberOfRows: Long = _maximumNumberOfRows
}

class OutputWriter(stream: OutputStream = System.out) {
  private val writer = new PrintWriter(new BufferedOutputStream(stream))

  def flush(): Unit = writer.flush()

  def println(value: Long): Unit = writer.println(value)
}

trait MergingTablesWork {

  def run(scanner: Scanner, writer: OutputWriter): Unit = {
    val n = scanner.nextInt()
    val m = scanner.nextInt()

    val tables = new TableDisJointSet(n, m)

    for (i <- 1 to n)
      tables.makeSet(i, scanner.nextLong())

    for (_ <- 0 until m) {
      val destination = scanner.nextInt()
      val source = scanner.nextInt()
      tables.merge(destination, source)
      writer.println(tables.maximumNumberOfRows)
    }
    writer.flush()
  }
}

object MergingTables extends App with MergingTablesWork {
  run(new Scanner(System.in), new OutputWriter())
}
