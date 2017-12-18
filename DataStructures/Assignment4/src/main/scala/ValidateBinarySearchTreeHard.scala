import java.util.Scanner

/**
  * @author sali
  */
trait ValidateBinarySearchTreeHardWork {

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

  private def readData(scanner: Scanner): Option[Node] = {
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
    tree.headOption
  }

  def isBinarySearchTree(root: Option[Node]): Boolean =
    isBinarySearchTree(root, equalAllowed = false, Long.MinValue, Long.MaxValue)

  private def isBinarySearchTree(mayBeRoot: Option[Node], equalAllowed: Boolean, min: Long, max: Long): Boolean =
    if (mayBeRoot.isEmpty) true
    else {
      val root = mayBeRoot.get
      val value = root.value

      val b = if (equalAllowed) value >= min && value <= max else value > min && value < max

      b && isBinarySearchTree(root.left, equalAllowed = false, min = min, max = value) && //
      isBinarySearchTree(root.right, equalAllowed = true, min = value, max = max)
    }

  def runTest(testIndex: Int): Unit = {
    val fileNamePrefix = f"$testIndex%02d"
    System.setIn(ValidateBinarySearchTreeHard.getClass.getResourceAsStream(s"/is-bst-hard/$fileNamePrefix"))
    solve()
  }

  def solve(): Unit = {
    val scanner = new Scanner(System.in)
    val runnable = new Runnable {
      override def run(): Unit = {
        val root = readData(scanner)
        if (isBinarySearchTree(root)) println("CORRECT") else println("INCORRECT")
      }
    }

    new Thread(null, runnable, "1", 1 << 26).start()
  }
}
object ValidateBinarySearchTreeHard extends App with ValidateBinarySearchTreeHardWork {
  solve()
}
