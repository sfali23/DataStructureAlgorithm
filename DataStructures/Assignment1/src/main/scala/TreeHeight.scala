import scala.collection.mutable
import scala.io.StdIn

trait TreeHeightWork {

  def computeHeight(root: Node): Int = {
    var height = Int.MinValue
    val queue = mutable.Queue[(Node, Int)]()
    queue.enqueue((root, 1))
    while (queue.nonEmpty) {
      val (current, level) = queue.dequeue()
      if (current.hasChildren)
        current.children.foreach(node => queue.enqueue((node, level + 1)))
      else height = height.max(level)
    }
    height
  }

  def readTree(n: Int, input: String): Node = {
    val nodes = Array.ofDim[Node](n)
    for (i <- nodes.indices) nodes(i) = new Node(i)

    var root: Node = new Node(-1)
    val values = input.split(" ")
    for (i <- values.indices) {
      val parentIndex = values(i).toInt
      if (parentIndex >= 0) nodes(i).parent = nodes(parentIndex)
      else root = nodes(i)
    }
    root
  }
}

class Node(val key: Int) {
  private var _parent: Node = _
  private var _children: List[Node] = List()

  def parent: Node = _parent
  def parent_=(parent: Node): Unit = {
    _parent = parent
    parent.addChild(this)
  }

  def addChild(node: Node): Unit = _children = node :: _children

  def children: List[Node] = _children

  def hasChildren: Boolean = _children.nonEmpty

  override def toString: String = key.toString
}

object TreeHeight extends App with TreeHeightWork {
  new Thread(null, () => {
    val n = StdIn.readInt()
    val input = StdIn.readLine()
    println(computeHeight(readTree(n, input)))
  }, "1", 1 << 26).start()
}
