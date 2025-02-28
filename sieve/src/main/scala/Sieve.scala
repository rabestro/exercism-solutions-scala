import scala.collection.mutable

object Sieve:
  def primes(number: Int): List[Int] =
    val unmarkedNumbers = mutable.BitSet(2 to number *)

    for (x <- unmarkedNumbers) do
      for (multiple <- x * x to number by x) do
        unmarkedNumbers -= multiple

    unmarkedNumbers.toList      
