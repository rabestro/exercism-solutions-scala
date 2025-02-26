import scala.collection.mutable

object Sieve:
  def primes(number: Int): List[Int] =
    val numbers = mutable.BitSet().addAll(2 to number)


    numbers.toList

          
