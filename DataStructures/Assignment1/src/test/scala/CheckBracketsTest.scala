import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterAll, FunSuite}
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
class DefaultCheckBracketsTest
    extends CheckBracketsTest
    with TestHelper
    with BeforeAndAfterAll {

  private var startTime: Long = 0L

  override protected def beforeAll(): Unit = {
    startTime = System.nanoTime()
  }

  override protected def afterAll(): Unit = {
    printTimeElapsed(startTime, "time to run all tests")
  }

  test("CheckBrackets: read pre-defined tests") {
    for (i <- 1 to 54) {
      val fileNamePrefix = f"$i%02d"
      val input = readLines(s"/check-brackets/$fileNamePrefix").head
      val output = readLines(s"/check-brackets/$fileNamePrefix.a").head
      assert(checkBracketWithAnswer(input) === output)
    }
  }
}
