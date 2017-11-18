import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * @author sali
  */
trait CheckBracketsTest extends FunSuite with CheckBracketsWork {

  test("CheckBrackets: Test case #1") {
    assert(checkBracket("[]") === 0)
  }

  test("CheckBrackets: Test case #2") {
    assert(checkBracket("{}[]") === 0)
  }

  test("CheckBrackets: Test case #3") {
    assert(checkBracket("[()]") === 0)
  }

  test("CheckBrackets: Test case #4") {
    assert(checkBracket("(())") === 0)
  }

  test("CheckBrackets: Test case #5") {
    assert(checkBracket("{[]}()") === 0)
  }

  test("CheckBrackets: Test case #6") {
    assert(checkBracket("{") === 1)
  }

  test("CheckBrackets: Test case #7") {
    assert(checkBracket("{[}") === 3)
  }

  test("CheckBrackets: Test case #8") {
    assert(checkBracket("foo(bar);") === 0)
  }

  test("CheckBrackets: Test case #9") {
    assert(checkBracket("foo[bar);") === 8)
  }

  test("CheckBrackets: Test case #10") {
    assert(checkBracket("foo(bar[i);") === 10)
  }

  test("CheckBrackets: Test case #11") {
    assert(checkBracket("([])[]()") === 0)
  }

  test("CheckBrackets: Test case #12") {
    assert(checkBracket("((([([])]))())") === 0)
  }

  test("CheckBrackets: Test case #13") {
    assert(checkBracket("([]]()") === 4)
  }

  test("CheckBrackets: Test case #14") {
    assert(checkBracket("][") === 1)
  }

  test("CheckBrackets: Test case #15") {
    assert(checkBracket("({[]") === 1, "({[]")
  }

  test("CheckBrackets: Test case #16") {
    assert(checkBracket("}") === 1)
  }

  test("CheckBrackets: Test case #17") {
    assert(checkBracket("(){[}") === 5)
  }

  test("CheckBrackets: Test case #18") {
    assert(checkBracket("[](()") === 3)
  }
}

@RunWith(classOf[JUnitRunner])
class DefaultCheckBracketsTest extends CheckBracketsTest with TestHelper {

  private val maxSize = 100000
  private val brackets = Array[Char]('-', '[', '{', '(', ']', '}', ')')

  private def fillArray(array: Array[Char],
                        start: Int,
                        end: Int): Array[Char] = {
    for (index <- start until end / 2) {
      val endIndex = end - 1 - index
      val i = generateInt(1, 4)
      array(index) = brackets(i)
      array(endIndex) = brackets(i + 3)
    }
    array
  }

  test("CheckBrackets: string with max length") {
    val str = fillArray(Array.ofDim[Char](maxSize), 0, maxSize).mkString
    println(str)
    assert(checkBracket(str) === 0)
  }

  test("CheckBrackets: string with max length with multiple sets") {
    var array = Array.ofDim[Char](maxSize)
    val size = maxSize / 4
    array = fillArray(array, 0, size)
    array = fillArray(array, 0, size)
    array = fillArray(array, 0, size)
    array = fillArray(array, 0, size)
    val str = array.mkString
    println(str)
    assert(checkBracket(str) === 0)
  }
}
