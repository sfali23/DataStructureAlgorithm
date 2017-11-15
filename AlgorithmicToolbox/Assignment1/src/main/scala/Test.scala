import scala.util.Random

/**
  * @author sali
  */
object Test extends App {

  val rand = Random

  /* for (_ <- 0 to 100) {
     val n = rand.nextInt(100) + 1
     val fibNum = Fibonacci.calc_fib_big(n)
     val lastNumber = fibNum.toString().last.toString.toLong
     val lastDigit = FibonacciLastDigit.getFibonacciLastDigit(n)
     val status = if (lastNumber == lastDigit) "OK" else "WRONG"
     println(s"$status, $n: $fibNum, $lastNumber, $lastDigit")
   }*/

  /*println(FibonacciLastDigit.getFibonacciLastDigit(331) == 9)
  println(FibonacciLastDigit.getFibonacciLastDigit(327305) == 5)
  println()

  println(GCD.gcd(1344, 217))
  println(GCD.gcd(217, 1344))
  println(GCD.gcd(35, 18))
  println(GCD.gcd(18, 35))
  println(GCD.gcd(28851538, 1183019))
  println()

  println(LCM.lcm(6, 8))
  println(LCM.lcm(28851538, 1183019))
  println()*/

  private val indexRange: Range.Inclusive = 0 to 30
  private val modRange: Range.Inclusive = 2 to 10
  displayFibonacciSeq(indexRange, modRange, Fibonacci.calc_fib_big)
  //displayFibonacciSeq(indexRange, modRange, FibonacciMatrix.fibonacciNumber)

  var sum = BigInt(0)
  for (n <- 0 to 28) {
    sum = sum + FibonacciMatrix.fibonacciNumber(BigInt(n))
  }

  println(s">>>> $sum")

  private val m = 10
  private val n = 200

  private val fibNumOfM: BigInt = FibonacciMatrix.fibonacciNumber(m)
  private val sumOfM: BigInt = FibonacciMatrix.fibonacciNumberSum(m)
  private val sumOfN: BigInt = FibonacciMatrix.fibonacciNumberSum(n)
  private val sum1 = sumOfN - sumOfM + fibNumOfM

  println(s"Fibonacci number from of $m: $fibNumOfM")
  println(s"Sum of Fibonacci number from 0 to $m: $sumOfM")
  println(s"Sum of Fibonacci number from 0 to $n: $sumOfN")
  println(s"SUM: $sum1")
  println()

  println(FibonacciPartialSumMatrix.fibonacciPartialSum(m, n))

  // println(s"Last digit of Fibonacci number of $n: ${FibonacciMatrix2.fibonacciNumberLastDigit(n)}")
  println(s"Last digit of sum of Fibonacci numbers 0 to $n: ${FibonacciMatrix.fibonacciNumberSumLastDigit(n)}")
  println(s"Last digit of sum of Fibonacci numbers 0 to $m: ${FibonacciSumLastDigitMatrix.fibonacciSumLastDigit(m)}")
  println(s"Last digit of sum of Fibonacci numbers 0 to $n: ${FibonacciSumLastDigitMatrix.fibonacciSumLastDigit(n)}")
  // println(s"Partial sum: ${FibonacciPartialSumMatrix.fibonacciPartialSum(m, n)}")

  /* println(FibonacciHuge.getFibonacciHuge(1, 239))
   println(FibonacciHuge.getFibonacciHuge(239, 1000))*/
  // println(FibonacciHuge.getFibonacciHuge(2816213588L, 30524L))
  // println(FibonacciHuge.getFibonacciHuge(99999999999999999L, 2L))

  private def fibonacciSeq(indexRange: Range, fib_calc: (Int => BigInt)): Seq[(Int, BigInt)] =
    for {
      n <- indexRange
      fn = fib_calc(n)
    } yield (n, fn)

  private def displayFibonacciSeq(indexRange: Range, modRange: Range, fib_calc: (Int => BigInt)): Unit = {
    val fibSeq = fibonacciSeq(indexRange, fib_calc)
    val fibonacciNumbers = fibSeq.map(_._2)

    printf("%-10s", "n")
    fibSeq.map(_._1).foreach(printf("%10s", _))
    println()
    println("-" * (fibSeq.size * 10 + 15))

    printf("%-10s", "Fn")
    fibSeq.map(_._2).foreach(printf("%10s", _))
    println()

    for (n <- modRange) {
      printFibonacciNumbers(fibonacciNumbers, n, s"Fn mod $n")
    }
    println()
    println("-" * (fibSeq.size * 10 + 15))
    println()
  }

  private def printFibonacciNumbers(fibonacciNumbers: Seq[BigInt], mod: Int, label: String): Unit = {
    printf("%-10s", label)
    fibonacciNumbers.map(_ % mod).foreach(printf("%10s", _))
    println()
  }

  private def exp_by_squaring(x: Double, n: Double): Double = exp_by_squaring(1, x, n)

  private def exp_by_squaring(y: Double, x: Double, n: Double): Double =
    if (n < 0) exp_by_squaring(y, 1 / x, n)
    else if (n == 0) y
    else if (n == 1) x * y
    else if (n % 2 == 0) exp_by_squaring(y, x * x, n / 2)
    else exp_by_squaring(x * y, x * x, (n - 1) / 2)

}
