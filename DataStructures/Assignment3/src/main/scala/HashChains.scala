/**
  * @author sali
  */
trait HashChainsWork {
  private val prime = 10000019
  private val multiplier = 263
  private val adder = 7

  private def hash(n: Int): Int = (multiplier * n + adder) % prime
}

object HashChains extends App with HashChainsWork {}
