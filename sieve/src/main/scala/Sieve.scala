import scala.collection.mutable

object Sieve:
  def primes(limit: Int): List[Int] =
    val unmarkedNumbers = mutable.BitSet(2 to limit *)

    for (x <- unmarkedNumbers) do
      for (multiple <- x * x to limit by x) do
        unmarkedNumbers -= multiple

    unmarkedNumbers.toList      
