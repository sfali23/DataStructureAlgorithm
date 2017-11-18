import scala.io.StdIn

case class Bracket(bracketType: Char, position: Int) {

  def isMatched(c: Char): Boolean = c match {
    case ']' if bracketType == '[' => true
    case '}' if bracketType == '{' => true
    case ')' if bracketType == '(' => true
    case _ => false
  }
}

trait CheckBracketsWork {

  def checkBracket(str: String): Int = {
    var stack = List[Bracket]()
    var fullyRead = true
    import scala.util.control.Breaks._
    breakable {
      for (position <- 0 until str.length) {
        val c = str.charAt(position)
        if (c == '[' || c == '{' || c == '(')
          stack = Bracket(c, position + 1) :: stack
        else if (c == ']' || c == '}' || c == ')') {
          if (stack.nonEmpty && stack.head.isMatched(c)) stack = stack.tail
          else {
            stack = Bracket(c, position + 1) :: stack
            fullyRead = false
            break
          }
        }
      }
    }
    if (stack.isEmpty) 0
    else if (fullyRead) stack.last.position
    else stack.head.position
  }
}

object CheckBrackets extends App with CheckBracketsWork {
  val result = checkBracket(StdIn.readLine())
  if (result == 0) println("Success") else println(result.toString)
}
