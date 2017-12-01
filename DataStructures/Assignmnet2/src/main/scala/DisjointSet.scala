/**
  * @author sali
  */
class DisjointSet(maxSize: Int) {
  private val parent: Array[Int] = Array.ofDim[Int](maxSize)
  private val rank: Array[Int] = Array.ofDim[Int](maxSize)

  def makeSet(i: Int): Unit = {
    if (i > maxSize) throw new IllegalArgumentException(s"Invalid value for i = $i")
    parent(i) = i
    rank(i) = 0
  }

  def makeSet(range: Range): Unit = range.foreach(makeSet)

  def findSet(i: Int): Int = {
    var index = i
    while (index != parent(i)) index = parent(i)
    index
  }

  def union(i: Int, j: Int): Unit = {
    val i_id = findSet(i)
    val j_id = findSet(j)
    if (i_id != j_id) {
      if (rank(i_id) > rank(j_id))
        parent(j_id) = i_id
      else {
        parent(i_id) = j_id
        if (rank(i_id) == rank(j_id))
          rank(j_id) = rank(j_id) + 1
      }
    }
  }

  def findSetWithPathCompression(i: Int): Int = {
    if (i != parent(i))
      parent(i) = findSetWithPathCompression(parent(i))
    parent(i)
  }

  def unionWithPathCompression(i: Int, j: Int): Unit = {
    val i_id = findSetWithPathCompression(i)
    val j_id = findSetWithPathCompression(j)
    if (i_id != j_id) {
      if (rank(i_id) > rank(j_id))
        parent(j_id) = i_id
      else {
        parent(i_id) = j_id
        if (rank(i_id) == rank(j_id))
          rank(j_id) = rank(j_id) + 1
      }
    }
  }

  def print(): Unit = {
    println(parent.tail.mkString("[", ", ", "]"))
    println(rank.tail.mkString("[", ", ", "]"))
    println()
  }
}

object DisjointSet {
  def apply(maxSize: Int): DisjointSet = {
    val ds = new DisjointSet(maxSize)
    ds.makeSet(1 until maxSize)
    ds
  }
}

object DisjointSetMain extends App {
  val ds = DisjointSet(13)
  ds.makeSet(1 to 12)
  ds.print()

  ds.union(2, 10)
  ds.union(7, 5)
  ds.union(6, 1)
  ds.union(3, 4)
  ds.union(5, 11)
  ds.union(7, 8)
  ds.union(7, 3)
  ds.union(12, 2)
  ds.union(9, 6)

  ds.print()

  println(ds.findSet(6))
  println(ds.findSet(3))
  println(ds.findSet(11))
  println(ds.findSet(9))

  println()

  val ds1 = DisjointSet(7)
  ds1.makeSet(1 to 6)
  ds1.print()

  ds1.unionWithPathCompression(2, 4)
  println("union(2, 4)")
  ds1.print()

  ds1.unionWithPathCompression(5, 2)
  println("union(5, 2)")
  ds1.print()

  ds1.unionWithPathCompression(3, 1)
  println("union(3, 1)")
  ds1.print()

  ds1.unionWithPathCompression(2, 3)
  println("union(2, 3)")
  ds1.print()

  ds1.unionWithPathCompression(2, 6)
  println("union(2, 6)")
  ds1.print()

  val n = 10
  val ds2 = DisjointSet(n + 1)
  ds2.makeSet(1 to n)
  for (i <- 1 until n)
    ds2.unionWithPathCompression(i, i + 1)
  ds2.print()

  question4()

  private def question4(): Unit = {
    val ds = DisjointSet(61)

    for(i <- 1 to 30) ds.unionWithPathCompression(i, 2 * i)
    for(i <- 1 to 20) ds.unionWithPathCompression(i, 3 * i)
    for(i <- 1 to 12) ds.unionWithPathCompression(i, 5 * i)
    for(i <- 1 to 60) ds.findSetWithPathCompression(i)
    ds.print()
  }
}
