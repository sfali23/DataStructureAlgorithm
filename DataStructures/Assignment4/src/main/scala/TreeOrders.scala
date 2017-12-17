import java.io.{ BufferedOutputStream, OutputStream, PrintWriter }
import java.util
import java.util.Scanner

/**
  * @author sali
  */
trait TreeOrdersWork {

  def readData(scanner: Scanner): Array[Node] = {
    val numOfVertices = scanner.nextInt()
    val tree = Array.ofDim[Node](numOfVertices)
    for (i <- 0 until numOfVertices) {
      var node = tree(i)
      if (node == null) {
        node = new Node()
        tree(i) = node
      }

      node.value = scanner.nextLong()
      node.index = i
      val indexOfLeftChild = scanner.nextInt()
      if (indexOfLeftChild > -1) {
        var child = tree(indexOfLeftChild)
        if (child == null) {
          child = new Node()
          child.index = indexOfLeftChild
          tree(indexOfLeftChild) = child
        }
        node.left = child
        child.parent = node
      }

      val indexOfRightChild = scanner.nextInt()
      if (indexOfRightChild > -1) {
        var child = tree(indexOfRightChild)
        if (child == null) {
          child = new Node()
          child.index = indexOfRightChild
          tree(indexOfRightChild) = child
        }
        node.right = child
        child.parent = node
      }

    }
    tree
  }

  def traversal(root: Option[Node], outputWriter: TreeOrdersOutputWriter): Unit = {
    inOrderTraversal(root, outputWriter)
    preOrderTraversal(root, outputWriter)
    postOrderTraversal(root, outputWriter)
  }

  private def inOrderTraversal(root: Option[Node], outputWriter: TreeOrdersOutputWriter): Unit = {
    val list = new util.ArrayList[Long]()
    inOrderTraversal(root, list)
    printList(list, outputWriter)
    outputWriter.flush()
  }

  private def preOrderTraversal(root: Option[Node], outputWriter: TreeOrdersOutputWriter): Unit = {
    val list = new util.ArrayList[Long]()
    preOrderTraversal(root, list)
    printList(list, outputWriter)
    outputWriter.flush()
  }

  private def postOrderTraversal(root: Option[Node], outputWriter: TreeOrdersOutputWriter): Unit = {
    val list = new util.ArrayList[Long]()
    postOrderTraversal(root, list)
    printList(list, outputWriter)
    outputWriter.flush()
  }

  private def inOrderTraversal(root: Option[Node], list: util.List[Long]): Unit = {
    if (root.isEmpty) {
      return
    }
    val node = root.get
    inOrderTraversal(node.left, list)
    list.add(node.value)
    inOrderTraversal(node.right, list)
  }

  private def preOrderTraversal(root: Option[Node], list: util.List[Long]): Unit = {
    if (root.isEmpty) {
      return
    }
    val node = root.get
    list.add(node.value)
    preOrderTraversal(node.left, list)
    preOrderTraversal(node.right, list)
  }

  private def postOrderTraversal(root: Option[Node], list: util.List[Long]): Unit = {
    if (root.isEmpty) {
      return
    }
    val node = root.get
    postOrderTraversal(node.left, list)
    postOrderTraversal(node.right, list)
    list.add(node.value)
  }

  private def printList(list: util.List[Long], outputWriter: TreeOrdersOutputWriter): Unit =
    if (!list.isEmpty) {
      outputWriter.print(list.get(0))
      for (i <- 1 until list.size()) {
        outputWriter.print(" ")
        outputWriter.print(list.get(i))
      }
      outputWriter.println()
    }

  def runTest(testIndex: Int): Unit = {
    val fileNamePrefix = f"$testIndex%02d"
    System.setIn(TreeOrders.getClass.getResourceAsStream(s"/tree-orders/$fileNamePrefix"))
    val scanner = new Scanner(System.in)
    val outputWriter = new TreeOrdersOutputWriter()
    val tree = readData(scanner)
    val runnable = new Runnable {
      override def run(): Unit =
        traversal(Some(tree.head), outputWriter)
    }

    new Thread(null, runnable, "1", 1 << 26).start()
  }

  def run(): Unit = {
    val scanner = new Scanner(System.in)
    val outputWriter = new TreeOrdersOutputWriter()
    val tree = readData(scanner)
    val runnable = new Runnable {
      override def run(): Unit =
        traversal(Some(tree.head), outputWriter)
    }

    new Thread(null, runnable, "1", 1 << 26).start()
  }
}

class Node() {
  private var _value: Long = 0L
  def value: Long = _value
  def value_=(v: Long): Unit = _value = v

  private var _index: Int = 0
  def index: Int = _index
  def index_=(i: Int): Unit = _index = i

  private var _left: Option[Node] = None
  def left: Option[Node] = _left
  def left_=(l: Node): Unit = _left = Some(l)

  private var _right: Option[Node] = None
  def right: Option[Node] = _right
  def right_=(r: Node): Unit = _right = Some(r)

  private var _parent: Option[Node] = None
  def parent: Option[Node] = _parent
  def parent_=(node: Node): Unit = _parent = Some(node)

}

class TreeOrdersOutputWriter(stream: OutputStream = System.out) {
  private val writer = new PrintWriter(new BufferedOutputStream(stream))

  def flush(): Unit = writer.flush()

  def close(): Unit = writer.close()

  def print(value: String): Unit = writer.print(value)

  def print(value: Long): Unit = writer.print(value)

  def println(): Unit = writer.println()
}

object TreeOrders extends App with TreeOrdersWork {
  run()
}
