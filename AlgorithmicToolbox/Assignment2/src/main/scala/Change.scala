import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait ChangeImpl {

  private val denominations = List(10, 5, 1)

  def getDenominations(m: Int): List[Int] = {
    val result = ListBuffer[Int]()
    if (m <= 0) Nil
    else {
      val availableDenominations = denominations.to[ListBuffer]
      var remainingValue = m
      while (remainingValue > 0 && availableDenominations.nonEmpty) {
        val currentDenomination = availableDenominations.head
        if (currentDenomination > remainingValue) {
          // current denomination can't be used
          // remove this denomination from availableDenominations
          availableDenominations -= currentDenomination
        } else {
          // there could be two cases:
          // either we have exact match, in this at the end of this loop remainingValue will be 0
          // or there will still be some value remained
          result += currentDenomination
          remainingValue -= currentDenomination
        }
      } // end of while
      result.to[List]
    } // end of else
  }

  def getChange(m: Int): Int = getDenominations(m).size

}

object Change extends App with ChangeImpl {
  val m = StdIn.readInt
  println(getChange(m))
}
